{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module GMMIO (
    GMMIn(..), pattern GMMIn,
    readInstance,
) where

import Control.Arrow (first)
import Control.DeepSeq (NFData)
import Data.Array.Accelerate (Elt, Arrays, Acc, Scalar, Vector, Matrix
                             ,Z(..), (:.)(..), pattern Pattern, Generic)
import qualified Data.Array.Accelerate as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lex.Fractional as BSLex
import Data.List (uncons)
import Data.Maybe (fromJust)

import Types


data GMMIn =
    GMMIn_ { gmmInAlphas   :: !(Vector FLT)  -- [k]
           , gmmInMeans    :: !(Matrix FLT)  -- [k][d]
           , gmmInICF      :: !(Matrix FLT)  -- [k][d + 1/2 d(d-1)]: d q's, then l's
           , gmmInX        :: !(Matrix FLT)  -- [n][d]
           , gmmInWisGamma :: !(Scalar FLT)
           , gmmInWisM     :: !(Scalar Int)
           }
  deriving (Generic)

deriving instance Show GMMIn
instance Arrays GMMIn
instance NFData GMMIn

pattern GMMIn :: Acc (Vector FLT)
              -> Acc (Matrix FLT)
              -> Acc (Matrix FLT)
              -> Acc (Matrix FLT)
              -> Acc (Scalar FLT)
              -> Acc (Scalar Int)
              -> Acc GMMIn
pattern GMMIn a mu i x g m = Pattern (a, mu, i, x, g, m)
{-# COMPLETE GMMIn #-}

readInstance :: FilePath -> IO GMMIn
readInstance fpath = do
    terms <- BS.words <$> BS.readFile fpath
    let bsReadInt   = fst . fromJust . BS.readInt
        bsReadFloat = fst . fromJust . BSLex.readSigned BSLex.readDecimal
        ([d, k, n], terms2) = first (map bsReadInt)   $ splitAt 3                              terms
        (alphas,    terms3) = first (map bsReadFloat) $ splitAt k                              terms2
        (means,     terms4) = first (map bsReadFloat) $ splitAt (k * d)                        terms3
        (icf,       terms5) = first (map bsReadFloat) $ splitAt (k * (d + d * (d-1) `div` 2))  terms4
        (x,         terms6) = first (map bsReadFloat) $ splitAt (n * d) terms5
        (gamma,     terms7) = first bsReadFloat       $ fromJust . uncons $                    terms6
        (m,         []    ) = first bsReadInt         $ fromJust . uncons $                    terms7
    let arr0D :: Elt a => a -> Scalar a
        arr0D = A.fromList Z . pure
        arr1D :: Elt a => Int -> [a] -> Vector a
        arr1D n1 = A.fromList (Z :. n1)
        arr2D :: Elt a => Int -> Int -> [a] -> Matrix a
        arr2D n1 n2 = A.fromList (Z :. n1 :. n2)
    return $ GMMIn_ { gmmInAlphas   = arr1D k alphas
                    , gmmInMeans    = arr2D k d means
                    , gmmInICF      = arr2D k (d + d * (d-1) `div` 2) icf
                    , gmmInX        = arr2D n d x
                    , gmmInWisGamma = arr0D gamma
                    , gmmInWisM     = arr0D m
                    }
