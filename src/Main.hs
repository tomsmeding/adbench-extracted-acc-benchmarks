{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forM, forM_)
import qualified Criterion.Measurement as CM
import qualified Criterion.Measurement.Types as CMT

import Data.Array.Accelerate (Acc, Vector, Matrix)
import qualified Data.Array.Accelerate.LLVM.Native as CPU

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

timeRun :: Program -> GMMIn -> IO TimeResult
timeRun prog input = do
    let compiled = CPU.run1 prog
    tm1 <- CMT.measTime . fst <$> CM.measure (CMT.whnf (`seq` ()) compiled) 1
    tm2 <- CMT.measTime . fst <$> CM.measure (CMT.nf compiled input) 1
    return (TimeResult tm1 tm2)

main :: IO ()
main = do
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

    forM_ programs $ \prog ->
        timeRun prog input >>= print
