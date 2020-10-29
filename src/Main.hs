{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad (forM)
import qualified Criterion.Measurement as CM
import qualified Criterion.Measurement.Types as CMT
import System.Environment

import Data.Array.Accelerate (Acc, Vector, Matrix, Arrays)
import qualified Data.Array.Accelerate as A
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
import Types


data Similarity
  = Similarity { simMaxAbsDiff :: Float
               , simMaxRelDiff :: Float
               , simMaxAbsVal :: Float
               , simTotal :: Float
               , simNumVals :: Int
               , simAverage :: Float }   -- recomputed every time from Total and NumVals
  deriving (Show)

instance Semigroup Similarity where
  Similarity mad mrd mav tot nv _ <> Similarity mad' mrd' mav' tot' nv' _ =
    Similarity (max mad mad') (max mrd mrd')
               (max mav mav') (tot + tot') (nv + nv')
               ((tot + tot') / fromIntegral (nv + nv'))

class Similar a where
  similarityReport :: a -> a -> Similarity

instance Similar [Float] where
  similarityReport l1 l2 =
    let absdiff x y = abs (x - y)
        reldiff x y = max ((x - y) / y) ((y - x) / x)
        total = sum (map abs l1) + sum (map abs l2)
        numvals = length l1 + length l2
    in Similarity { simMaxAbsDiff = maximum (zipWith absdiff l1 l2)
                  , simMaxRelDiff = maximum (zipWith reldiff l1 l2)
                  , simMaxAbsVal = max (maximum l1) (maximum l2)
                  , simTotal = total
                  , simNumVals = numvals
                  , simAverage = total / fromIntegral numvals }

instance A.Shape sh => Similar (A.Array sh Float) where
  similarityReport a1 a2 = similarityReport (A.toList a1) (A.toList a2)

instance (Similar a, Similar b, Similar c) => Similar (a, b, c) where
  similarityReport (a, b, c) (x, y, z) =
    similarityReport a x <> similarityReport b y <> similarityReport c z


type Output = (Vector Float, Matrix Float, Matrix Float)
type Program = Acc GMMIn -> Acc Output

data TimeResult = TimeResult { tmCompile :: Double
                             , tmCompute :: Double }
  deriving (Show)

run1For :: (Arrays a, Arrays b) => BackendKind -> (Acc a -> Acc b) -> a -> b
run1For Interpreter = I.run1
run1For CPU = CPU.run1
run1For GPU = GPU.run1

timeRun :: Options -> Program -> GMMIn -> IO (TimeResult, Output)
timeRun options prog input = do
    debugLn options "Compiling..."
    let compiled = run1For (optBackend options) prog
    debugLn options "Timing compilation force..."
    tm1 <- CMT.measTime . fst <$> CM.measure (CMT.whnf (`seq` ()) compiled) 1
    debugLn options "Timing apply force..."
    let output = compiled input
    tm2 <- CMT.measTime . fst <$> CM.measure (CMT.whnf (`seq` ()) output) 1
    return (TimeResult tm1 tm2, output)

data Options = Options { optBackend :: BackendKind
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

    outs <- forM (zip [1::Int ..] programs) $ \(i, prog) -> do
        debugLn options ("Running program " ++ show i)
        (tm, out) <- timeRun options prog input
        print tm
        return out

    mapM_ (print . similarityReport (head outs)) (tail outs)
