------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.Reproduction.Sexual
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A reproduction method for artificial lifeforms where:
--
-- * Each agent has /two/ strands of genetic information.
--
-- * Each child has two parents.
--
-- * Each parent contributes approximately half of its genetic
--   information to the offspring.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Genetics.Reproduction.Sexual
  (
    Reproductive(..)
  ) where

import ALife.Creatur (AgentId)
import Control.Monad.Random (Rand, RandomGen)

-- | Full set (both strands) of genetic information for an organism.
type Genome a = (Strand a, Strand a)


-- | A species that reproduces, transmitting genetic information to
--   its offspring. Minimal complete definition: all except @mate@.
class Reproductive a where

  -- | A sequence of genetic information for an agent.
  type Strand a

  -- | Returns the full set (both strands) of genetic information for an
  --   organism.
  genome :: a -> Genome a

  -- | From the /two/ strands of the genetic information from this
  --   agent, creates a /single/ strand that will contribute to the
  --   child's genome.
  --   (This is analogous to creating either a single sperm or ova.)
  produceGamete :: RandomGen r => a -> Rand r (Strand a)

  -- | Builds an agent based on the genome provided, if it is possible
  --   to do so.
  build :: AgentId -> Genome a -> Either [String] a

  -- | @'makeOffspring' (parent1, parent2) name@ uses the genetic
  --   information from @parent1@ and @parent2@ to produce a child with
  --   the agent ID @name@. The default implementation:
  --
  --   1. Calls @'produceGamete'@ to produce a single strand of genetic
  --      information from each parent.
  --
  --   1. Pairs the two strands to create a genome for the child.
  --
  --   1. Calls @'build'@ construct a child with this genome.
  makeOffspring
    :: RandomGen r
      => a -> a -> AgentId -> Rand r (Either [String] a)
  makeOffspring a b name = do
    ga <- produceGamete a
    gb <- produceGamete b
    return $ build name (ga, gb)

  clone :: a -> AgentId -> a
  clone a name = a'
    -- It must be possible to create an agent from this genome; it has
    -- already been done.
    where (Right a') = build name $ genome a
