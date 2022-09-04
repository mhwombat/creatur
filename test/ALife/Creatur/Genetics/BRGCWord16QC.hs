------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.BRGCWord16QC
-- Copyright   :  (c) 2014-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module ALife.Creatur.Genetics.BRGCWord16QC
  (
    test
  ) where

import           ALife.Creatur.Genetics.Analysis      (Analysable)
import           ALife.Creatur.Genetics.BRGCWord16
import           Data.Word                            (Word16, Word8)
import           GHC.Generics                         (Generic)
import           Prelude                              hiding (read)
import           Test.Framework                       as TF (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      (Arbitrary, Gen,
                                                       arbitrary, choose, oneof,
                                                       sized, vectorOf)

prop_round_trippable :: (Eq g, Genetic g) => g -> Bool
prop_round_trippable g = g' == Right g
  where x = write g
        g' = read x

data TestStructure = A | B Bool | C Word8 | D Word16 Char | E [TestStructure]
  deriving (Read, Show, Eq, Generic)

instance Genetic TestStructure
instance Analysable TestStructure

sizedArbTestStructure :: Int -> Gen TestStructure
sizedArbTestStructure 0 =
  oneof [ return A, B <$> arbitrary, C <$> arbitrary,
          D <$> arbitrary <*> arbitrary]
sizedArbTestStructure n = do
  k <- choose (0,min 8 (n-1))
  oneof [
          return A,
          B <$> arbitrary,
          C <$> arbitrary,
          D <$> arbitrary <*> arbitrary,
          E <$> vectorOf k (sizedArbTestStructure (n-1))
        ]

instance Arbitrary TestStructure where
  arbitrary = sized sizedArbTestStructure

test :: Test
test = testGroup "ALife.Creatur.Genetics.BRGCWord16QC"
  [
    testProperty "prop_round_trippable - Bool"
      (prop_round_trippable :: Bool -> Bool),
    testProperty "prop_round_trippable - Char"
      (prop_round_trippable :: Char -> Bool),
    testProperty "prop_round_trippable - Word8"
      (prop_round_trippable :: Word8 -> Bool),
    testProperty "prop_round_trippable - Word16"
      (prop_round_trippable :: Word16 -> Bool),
    -- testProperty "prop_round_trippable - Word32"
    --   (prop_round_trippable :: Word32 -> Property),
    -- testProperty "prop_round_trippable - Word64"
    --   (prop_round_trippable :: Word64 -> Property),
    testProperty "prop_round_trippable - Int"
      (prop_round_trippable :: Int -> Bool),
    -- testProperty "prop_round_trippable - Integer"
    --   (prop_round_trippable :: Integer -> Property),
    -- testProperty "prop_round_trippable - Double"
    --   (prop_round_trippable :: Double -> Property),
    testProperty "prop_round_trippable - TestStructure"
      (prop_round_trippable :: TestStructure -> Bool)
  ]


