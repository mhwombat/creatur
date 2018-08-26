------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.Reproduction.SimplifiedSexual
-- Copyright   :  (c) Amy de Buitléir 2012-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A reproduction method for artificial lifeforms where:
--
-- * Each agent has a /single/ strand of genetic information.
--
-- * Each child has two parents.
--
-- * Each parent contributes approximately half of its genetic
--   information to the offspring.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Genetics.Reproduction.SimplifiedSexual
  (
    Reproductive(..)
  ) where

import ALife.Creatur (AgentId)
import Control.Monad.Random (Rand, RandomGen)

-- | A species that reproduces, transmitting genetic information to
--   its offspring. Minimal complete definition: all except @mate@.
class Reproductive a where

  -- | A sequence of hereditary information for an agent.
  type Strand a

  -- | Recombines the genetic information from two parents, creating
  --   genetic information for potential offspring.
  --
  --   Typically this involves the following steps:
  --
  --   1. Recombine the two strands of genetic information (one from
  --      each parent) to obtain two new strands.
  --
  --   1. Discard one strand, and return the remaining one.
  recombine :: RandomGen r => a -> a -> Rand r (Strand a)

  -- | Builds an agent based on the genome provided, if it is possible
  --   to do so.
  build :: AgentId -> Strand a -> Either [String] a

  -- | @'makeOffspring' (parent1, parent2) name@ uses the genetic
  --   information from @parent1@ and @parent2@ to produce a child with
  --   the agent ID @name@. The default implementation:
  --
  --   1. Calls @'recombine'@ to create a genome for the child.
  --
  --   2. Calls @'build'@ to construct a child with this genome.
  makeOffspring
    :: RandomGen r
      => a -> a -> AgentId -> Rand r (Either [String] a)
  makeOffspring a b name = do
    g <- recombine a b
    return $ build name g



