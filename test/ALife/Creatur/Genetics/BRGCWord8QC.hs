------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.BRGCWord8QC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module ALife.Creatur.Genetics.BRGCWord8QC
  (
    test
  ) where

import           ALife.Creatur.Genetics.Analysis      (Analysable)
import           ALife.Creatur.Genetics.BRGCWord8
import           ALife.Creatur.Util                   (fromEither)
import           Data.Word                            (Word16, Word32, Word64,
                                                       Word8)
import           GHC.Generics                         (Generic)
import           Prelude                              hiding (read)
import           Test.Framework                       as TF (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      (Arbitrary, Gen, Property,
                                                       arbitrary, choose, oneof,
                                                       sized, vectorOf, (==>))

#if MIN_VERSION_base(4,8,0)
#else
import           Control.Applicative
#endif

prop_round_trippable :: (Eq g, Genetic g) => g -> Bool
prop_round_trippable g = g' == Right g
  where x = write g
        g' = read x

prop_round_trippable2
  :: Genetic g => Int -> [Word8] -> g -> Property
prop_round_trippable2 n xs dummy = length xs >= n
  ==> xs' == take n xs
  where Right g = read xs
        xs' = write (g `asTypeOf` dummy)

prop_rawWord8s_round_trippable :: [Word8] -> Bool
prop_rawWord8s_round_trippable g = g' == g
  where x = fst $ runWriter (putRawWord8s g)
        g' = fromEither (error "read returned Nothing") .
               fst . runReader (getRawWord8s n) $ x
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
      (prop_round_trippable :: Bool -> Bool),
    testProperty "prop_round_trippable - Char"
      (prop_round_trippable :: Char -> Bool),
    testProperty "prop_round_trippable - Word8"
      (prop_round_trippable :: Word8 -> Bool),
    testProperty "prop_round_trippable - Word16"
      (prop_round_trippable :: Word16 -> Bool),
    testProperty "prop_round_trippable - Word32"
      (prop_round_trippable :: Word32 -> Bool),
    testProperty "prop_round_trippable - Word64"
      (prop_round_trippable :: Word64 -> Bool),
    testProperty "prop_round_trippable - Int"
      (prop_round_trippable :: Int -> Bool),
    testProperty "prop_round_trippable - Integer"
      (prop_round_trippable :: Integer -> Bool),
    testProperty "prop_round_trippable - Double"
      (prop_round_trippable :: Double -> Bool),
    testProperty "prop_round_trippable2 - Word8"
      (prop_round_trippable2 1 :: [Word8] -> Word8 -> Property),
    testProperty "prop_round_trippable2 - Word16"
      (prop_round_trippable2 2 :: [Word8] -> Word16 -> Property),
    testProperty "prop_round_trippable2 - Word32"
      (prop_round_trippable2 4 :: [Word8] -> Word32 -> Property),
    testProperty "prop_round_trippable2 - Word64"
      (prop_round_trippable2 8 :: [Word8] -> Word64 -> Property),
    testProperty "prop_round_trippable - TestStructure"
      (prop_round_trippable :: TestStructure -> Bool),
    testProperty "prop_rawWord8s_round_trippable"
      prop_rawWord8s_round_trippable
  ]


