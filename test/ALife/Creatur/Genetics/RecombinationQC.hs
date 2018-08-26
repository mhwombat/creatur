------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.RecombinationQC
-- Copyright   :  (c) Amy de Buitléir 2012-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Genetics.RecombinationQC
  (
    test
  ) where

import ALife.Creatur.Genetics.Recombination (crossover, cutAndSplice)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, property)

prop_cutAndSplice_preserves_sum_of_lengths ::
  Int -> Int -> (String, String) -> Property
prop_cutAndSplice_preserves_sum_of_lengths n m (as, bs) =
  property $ length as' + length bs' == length as + length bs
    where (as', bs') = cutAndSplice n m (as, bs)

prop_crossover_preserves_sum_of_lengths :: Int -> (String, String) -> Property
prop_crossover_preserves_sum_of_lengths n (as, bs) =
  property $ length as' + length bs' == length as + length bs
    where (as', bs') = crossover n (as, bs)

test :: Test
test = testGroup "QuickCheck ALife.Creatur.Genetics.RecombinationQC"
  [
    testProperty "prop_cutAndSplice_preserves_sum_of_lengths"
      prop_cutAndSplice_preserves_sum_of_lengths,
    testProperty "prop_crossover_preserves_sum_of_lengths"
      prop_crossover_preserves_sum_of_lengths
  ]
