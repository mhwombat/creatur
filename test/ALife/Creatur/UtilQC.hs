------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.UtilQC
-- Copyright   :  (c) Amy de Buitléir 2012-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.UtilQC
  (
    test
  ) where

import ALife.Creatur.Util (cropRect, cropSquare, isqrt, replaceElement, 
  safeReplaceElement, shuffle)
import Control.Monad.Random (evalRand, mkStdGen)
import Data.Ix (range)
import Data.List (sort)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, property)

-- prop_constrain_obeys_bounds :: (Int, Int) -> Int -> Property
-- prop_constrain_obeys_bounds (a, b) x = property $ a <= x' && x' <= b
--  where x' = constrain (a, b) x

-- prop_constrain_works :: (Int, Int) -> Int -> Property
-- prop_constrain_works (a, b) x = property $ x' == x || x <= a || b <= x || b < a
--  where x' = constrain (a, b) x

prop_cropRect_returns_correct_size ::
  (Int, Int) -> (Int, Int) -> String -> Int -> Property
prop_cropRect_returns_correct_size (a,b) (c,d) xs k =
    property $ length xs' == expectedSize || length xs < minimalOriginalSize
  where expectedSize = length is'
        is = range ((0,0),(lastRow,lastCol))
        is' = filter wanted is
        wanted (i,j) = a <= i && i <= c && b <= j && j <= d
        lastRow = if k == 0 then -1 else (length xs `div` k) - 1 + delta
        lastCol = constrain (-1,length xs - 1) (k - 1)
        delta = if length xs `mod` k == 0 then 0 else 1 --add partial row
        xs' = cropRect (a, b) (c, d) xs k
        minimalOriginalSize = c*k + d + 1
          -- the last row of the original matrix must have at least
          -- d+1 elements

-- Warning: If b < a, returns either a or x
constrain :: Ord a => (a, a) -> a -> a
constrain (a,b) x | x < a     = a
                  | x > b     = b
                  | otherwise = x

prop_cropSquare_returns_correct_size :: Int -> String -> Property
prop_cropSquare_returns_correct_size n xs =
    property $ length xs' == expectedSize
  where expectedRows = min n ((isqrt . length) xs)
        expectedSize = if n < 0 then 0 else expectedRows*expectedRows
        xs' = cropSquare n xs

prop_replaceElement_changes_the_right_element :: String -> Int -> Char -> Property
prop_replaceElement_changes_the_right_element cs i c =
  property
    $
      if 0 <= i && i < length cs
        then replaceElement cs i c !! i == c
        else replaceElement cs i c == cs

prop_safeReplaceElement_doesnt_change_length :: String -> Int -> Char -> Property
prop_safeReplaceElement_doesnt_change_length cs i c =
  property $ length cs == length (safeReplaceElement cs i c)

prop_safeReplaceElement_changes_the_right_element ::
  String -> Int -> Char -> Property
prop_safeReplaceElement_changes_the_right_element cs i c =
  property
    $
      if 0 <= i && i < length cs
        then cs' !! i == c
        else cs' == cs
  where cs' = safeReplaceElement cs i c

prop_shuffle_doesnt_change_elements :: String -> Int -> Property
prop_shuffle_doesnt_change_elements xs k = property $ sort xs == sort xs'
  where xs' = evalRand (shuffle xs) (mkStdGen k)

test :: Test
test = testGroup "QuickCheck ALife.Creatur.UtilQC"
  [
--    testProperty "prop_constrain_obeys_bounds"
--      prop_constrain_obeys_bounds,
--    testProperty "prop_constrain_works"
--      prop_constrain_works,
    testProperty "prop_safeReplaceElement_changes_the_right_element"
      prop_safeReplaceElement_changes_the_right_element,
    testProperty "prop_safeReplaceElement_doesnt_change_length"
      prop_safeReplaceElement_doesnt_change_length,
    testProperty "prop_cropRect_returns_correct_size"
      prop_cropRect_returns_correct_size,
    testProperty "prop_cropSquare_returns_correct_size"
      prop_cropSquare_returns_correct_size,
    testProperty "prop_replaceElement_changes_the_right_element"
      prop_replaceElement_changes_the_right_element,
    testProperty "prop_shuffle_doesnt_change_elements"
      prop_shuffle_doesnt_change_elements
  ]
