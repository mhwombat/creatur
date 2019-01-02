------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.DiploidQC
-- Copyright   :  (c) Amy de Buitléir 2013-2019
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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Genetics.DiploidQC
  (
    test
  ) where

import ALife.Creatur.Genetics.Diploid
import GHC.Generics (Generic)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, choose,
  oneof, property, sized, vectorOf)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif

data TestStructure = A | B Bool | C Int | D Bool Char | E [TestStructure]
  deriving (Show, Eq, Generic)

instance Diploid TestStructure

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

prop_identity :: TestStructure -> Property
prop_identity g = property $ express g g == g


test :: Test
test = testGroup "ALife.Creatur.Genetics.DiploidQC"
  [
    testProperty "prop_identity" prop_identity
  ]


