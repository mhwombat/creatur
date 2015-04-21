------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.BRGCWord8QC
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module ALife.Creatur.Genetics.BRGCWord8QC
  (
    test
  ) where

import Prelude hiding (read)
import ALife.Creatur.Genetics.BRGCWord8
import ALife.Creatur.Genetics.Analysis (Analysable)
import ALife.Creatur.Util (fromEither)
import Data.Word (Word8, Word16)
import GHC.Generics (Generic)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, choose,
  oneof, property, sized, vectorOf)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif

prop_round_trippable :: (Eq g, Genetic g) => g -> Property
prop_round_trippable g = property $ g' == Right g
  where x = write g
        g' = read x

prop_rawWord8s_round_trippable :: [Word8] -> Property
prop_rawWord8s_round_trippable g = property $ g' == g
  where x = runWriter (putRawWord8s g)
        g' = fromEither (error "read returned Nothing") .
               runReader (getRawWord8s n) $ x
        n = length g

data TestStructure = A | B Bool | C Word8 | D Word16 Char | E [TestStructure]
  deriving (Show, Eq, Generic)

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
test = testGroup "ALife.Creatur.Genetics.BRGCWord8QC"
  [
    testProperty "prop_round_trippable - Bool"
      (prop_round_trippable :: Bool -> Property),
    testProperty "prop_round_trippable - Char"
      (prop_round_trippable :: Char -> Property),
    testProperty "prop_round_trippable - Word8"
      (prop_round_trippable :: Word8 -> Property),
    testProperty "prop_round_trippable - Word16"
      (prop_round_trippable :: Word16 -> Property),
    testProperty "prop_round_trippable - TestStructure"
      (prop_round_trippable :: TestStructure -> Property),
    testProperty "prop_rawWord8s_round_trippable"
      prop_rawWord8s_round_trippable
  ]


