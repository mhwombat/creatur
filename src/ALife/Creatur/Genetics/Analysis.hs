------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.Analysis
-- Copyright   :  (c) Amy de Buitléir 2013-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances,
    DefaultSignatures, DeriveGeneric, TypeOperators #-}
module ALife.Creatur.Genetics.Analysis
  (
    Analysable(..)
  ) where

import Data.Word (Word8, Word16)
import GHC.Generics

class Analysable g where
  -- | Analyses a genetic sequence.
  analyse :: g -> String

  default analyse :: (Generic g, GAnalysable (Rep g)) => g -> String
  analyse = ganalyse . from

class GAnalysable f where
  ganalyse :: f a -> String

-- | Unit: used for constructors without arguments
instance GAnalysable U1 where
  ganalyse U1 = "U1 "

-- | Constants, additional parameters and recursion of kind *
instance (GAnalysable a, GAnalysable b) => GAnalysable (a :*: b) where
  ganalyse (a :*: b) = ganalyse a ++ ":*: " ++ ganalyse b

-- | Meta-information (constructor names, etc.)
instance (GAnalysable a, GAnalysable b) => GAnalysable (a :+: b) where
  ganalyse (L1 x) = "L1 " ++ ganalyse x
  ganalyse (R1 x) = "R1 " ++ ganalyse x

-- | Sums: encode choice between constructors
instance (GAnalysable a) => GAnalysable (M1 i c a) where
  ganalyse (M1 x) = "M1 " ++ ganalyse x

-- | Products: encode multiple arguments to constructors
instance (Analysable a) => GAnalysable (K1 i a) where
  ganalyse (K1 x) = "K1 " ++ analyse x

--
-- Instances
--

instance Analysable Bool where
  analyse x = show x ++ " "

instance Analysable Char where
  analyse x = show x ++ " "

instance Analysable Word8 where
  analyse x = show x ++ " "

instance Analysable Word16 where
  analyse x = show x ++ " "

instance (Analysable a) => Analysable [a]

instance (Analysable a) => Analysable (Maybe a)

instance (Analysable a, Analysable b) => Analysable (a, b)

instance (Analysable a, Analysable b) => Analysable (Either a b)
