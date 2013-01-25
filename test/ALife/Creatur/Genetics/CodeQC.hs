{-# LANGUAGE UnicodeSyntax #-}

module ALife.Creatur.Genetics.CodeQC
  (
    test
  ) where

import ALife.Creatur.Genetics.Code
import ALife.Creatur.Genetics.CodeInternal
import Data.Eq.Unicode ((≡))
import Data.Ord.Unicode ((≤))
import Data.List (nub)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, property, 
  sized, vectorOf)

-- guaranteed not to have multiple values for the same code (but might have
-- multiple codes for the same value)
data TestCode = TestCode (Code Char Bool) deriving Show

sizedArbTestCode ∷ Int → Gen TestCode
sizedArbTestCode n = do
  cs ← vectorOf n arbitrary
  xs ← vectorOf n arbitrary
  return $ TestCode $ Code n $ zip cs (nub xs)

instance Arbitrary TestCode where
  arbitrary = sized sizedArbTestCode

prop_encoding_round_trippable ∷ TestCode → Char → Property
prop_encoding_round_trippable (TestCode g) c =
  property $ maybe Nothing (decode g) (encode g c) ≡ c'
    where c' = if c `elem` cs then Just c else Nothing
          cs = map fst $ cTable g

-- guaranteed not to have multiple values for the same code, or multiple codes
-- for the same value
data TestCode2 = TestCode2 (Code Char Bool) deriving Show

sizedArbTestCode2 ∷ Int → Gen TestCode2
sizedArbTestCode2 n = do
  cs ← vectorOf n arbitrary
  xs ← vectorOf n arbitrary
  return $ TestCode2 $ Code n $ zip (nub cs) (nub xs)

instance Arbitrary TestCode2 where
  arbitrary = sized sizedArbTestCode2

prop_decoding_round_trippable ∷ TestCode2 → [Bool] → Property
prop_decoding_round_trippable (TestCode2 g) x =
  property $ maybe Nothing (encode g) (decode g x) ≡ x'
    where x' = if x `elem` xs then Just x else Nothing
          xs = map snd $ cTable g

data TestParms = TestParms String deriving Show

sizedTestParms ∷ Int → Gen TestParms
sizedTestParms n = do
  let n' = 2 + min 8 n -- keep number of values low to speed up tests
  let values = take n' ['a' .. 'z']
  return $ TestParms values

instance Arbitrary TestParms where
  arbitrary = sized sizedTestParms
  
prop_gray_code_is_efficient ∷ TestParms → Property
prop_gray_code_is_efficient (TestParms values) = 
  property $ 2 ^ (nBits - 1) < nValues && nValues ≤ 2 ^ nBits
    where g = mkGrayCode values
          nValues = length values
          nBits = (length . snd . head . cTable) g

test ∷ Test
test = testGroup "QuickCheck ALife.Creatur.Genetics.CodeQC"
  [
    testProperty "prop_encoding_round_trippable"
      prop_encoding_round_trippable,
    testProperty "prop_decoding_round_trippable"
      prop_decoding_round_trippable,
    testProperty "prop_gray_code_is_efficient"
      prop_gray_code_is_efficient
  ]


