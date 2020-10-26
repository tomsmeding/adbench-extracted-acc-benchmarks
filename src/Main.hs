{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forM_)
import qualified Criterion.Measurement as CM
import qualified Criterion.Measurement.Types as CMT
import System.Environment

import Data.Array.Accelerate (Acc, Vector, Matrix, Arrays)
import qualified Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate.LLVM.Native as CPU
import qualified Data.Array.Accelerate.LLVM.PTX as GPU

import GMMIO
import M1 (inputProgram)
import M1N (withDefaultBackpermute)
import M2 (simplified)
import M3 (withShapeProp)
import M4 (withoutExpTemps)
import M4a (selectiveRecompute1)
import M4b (selectiveRecompute2)
import M4N (withoutExpTempsN)


type Program = Acc GMMIn -> Acc (Vector Float, Matrix Float, Matrix Float)

data TimeResult = TimeResult { tmCompile :: Double
                             , tmCompute :: Double }
  deriving (Show)

data Backend = Interpreter | CPU | GPU
  deriving (Show)

run1For :: (Arrays a, Arrays b) => Backend -> (Acc a -> Acc b) -> a -> b
run1For Interpreter = I.run1
run1For CPU = CPU.run1
run1For GPU = GPU.run1

timeRun :: Options -> Program -> GMMIn -> IO TimeResult
timeRun options prog input = do
    debugLn options "Compiling..."
    let compiled = run1For (optBackend options) prog
    debugLn options "Timing compilation force..."
    tm1 <- CMT.measTime . fst <$> CM.measure (CMT.whnf (`seq` ()) compiled) 1
    debugLn options "Timing apply force..."
    tm2 <- CMT.measTime . fst <$> CM.measure (CMT.nf compiled input) 1
    return (TimeResult tm1 tm2)

data Options = Options { optBackend :: Backend
                       , optDebug :: Bool }
  deriving (Show)

defaultOptions :: Options
defaultOptions = Options CPU False

parseOption :: String -> Options -> Options
parseOption "-inter" o = o { optBackend = Interpreter }
parseOption "-cpu"   o = o { optBackend = CPU }
parseOption "-gpu"   o = o { optBackend = GPU }
parseOption "-debug" o = o { optDebug = True }
parseOption opt      _ = error ("Unrecognised option '" ++ opt ++ "'")

debugLn :: Options -> String -> IO ()
debugLn (Options { optDebug = True }) = putStrLn
debugLn _ = const (return ())

main :: IO ()
main = do
    options <- foldr parseOption defaultOptions <$> getArgs

    let programs :: [Program]
        programs = 
            [inputProgram
            ,withDefaultBackpermute
            ,simplified
            ,withShapeProp
            ,withoutExpTemps
            ,selectiveRecompute1
            ,selectiveRecompute2
            ,withoutExpTempsN]

    -- For more data files, all files in this directory should work (though may
    -- be slow for the large ones):
    --   https://github.com/microsoft/ADBench/tree/master/data/gmm
    -- I've only tested with the 1k and 10k directories.
    input <- readInstance "gmm_1k_d32_K25.txt"

    forM_ (zip [1::Int ..] programs) $ \(i, prog) -> do
        debugLn options ("Running program " ++ show i)
        timeRun options prog input >>= print
