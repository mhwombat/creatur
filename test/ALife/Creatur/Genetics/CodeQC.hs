------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.CodeQC
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Genetics.CodeQC
  (
    test
  ) where

import ALife.Creatur.Genetics.Code
import Data.List (nub)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, property, 
  sized, vectorOf)

-- Guaranteed not to have multiple values for the same code (but might
-- have multiple codes for the same value). Compare with TestCode2.
data TestCode = TestCode (Code Char Bool) deriving Show

sizedArbTestCode :: Int -> Gen TestCode
sizedArbTestCode n = do
  cs <- vectorOf n arbitrary
  xs <- vectorOf n arbitrary
  return $ TestCode $ Code n $ zip cs (nub xs)

instance Arbitrary TestCode where
  arbitrary = sized sizedArbTestCode

prop_encoding_round_trippable :: TestCode -> Char -> Property
prop_encoding_round_trippable (TestCode g) c =
  property $ maybe Nothing (decode g) (encode g c) == c'
    where c' = if c `elem` cs then Just c else Nothing
          cs = map fst $ cTable g

-- Guaranteed not to have multiple values for the same code, or multiple
-- codes for the same value. Compare with TestCode.
data TestCode2 = TestCode2 (Code Char Bool) deriving Show

sizedArbTestCode2 :: Int -> Gen TestCode2
sizedArbTestCode2 n = do
  cs <- vectorOf n arbitrary
  xs <- vectorOf n arbitrary
  return $ TestCode2 $ Code n $ zip (nub cs) (nub xs)

instance Arbitrary TestCode2 where
  arbitrary = sized sizedArbTestCode2

prop_decoding_round_trippable :: TestCode2 -> [Bool] -> Property
prop_decoding_round_trippable (TestCode2 g) x =
  property $ maybe Nothing (encode g) (decode g x) == x'
    where x' = if x `elem` xs then Just x else Nothing
          xs = map snd $ cTable g

test :: Test
test = testGroup "QuickCheck ALife.Creatur.Genetics.CodeQC"
  [
    testProperty "prop_encoding_round_trippable"
      prop_encoding_round_trippable,
    testProperty "prop_decoding_round_trippable"
      prop_decoding_round_trippable
  ]


