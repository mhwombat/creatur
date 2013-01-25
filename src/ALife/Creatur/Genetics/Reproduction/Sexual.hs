------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.Reproduction.Sexual
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A reproduction method for artificial lifeforms where:
-- * Each agent has /two/ strands of genetic information.
-- * Each child has two parents.
-- * Each parent contributes approximately half of its genetic
--   information to the offspring.
--
------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax, TypeFamilies #-}
module ALife.Creatur.Genetics.Reproduction.Sexual
  (
    Reproductive(..)
  ) where

import ALife.Creatur (AgentId)
import Control.Monad.Random (Rand, RandomGen)

-- | A species that reproduces, transmitting genetic information to
--   its offspring. Minimal complete definition: all except @mate@.
class Reproductive a where

  -- | The basic unit of hereditary information for an agent.
  --   The type signature for the agent's genome is 
  --   ([Base a], [Base a]).
  type Base a

  -- | From the /two/ strands of the genetic information from this 
  --   agent, creates a /single/ strand that will contribute to the
  --   child's genome. 
  --   (This is analogous to creating either a single sperm or ova.)
  produceGamete ∷ RandomGen r ⇒ a → Rand r [Base a]

  -- | Builds an agent based on the genome provided, if it is possible
  --   to do so.
  build ∷ AgentId → ([Base a], [Base a]) → Maybe a

  -- | @'makeOffspring' (parent1, parent2) name@ uses the genetic
  --   information from @parent1@ and @parent2@ to produce a child with
  --   the agent ID @name@.
  --   The default implementation:
  --   1. Calls @'produceGamete'@ to produce a single strand of genetic
  --      information from each parent.
  --   2. Pairs the two strands to create a genome for the child.
  --   3. Calls @'build' construct a child with this genome.
  makeOffspring ∷ RandomGen r ⇒ a → a → AgentId → Rand r (Maybe a)
  makeOffspring a b name = do
    ga ← produceGamete a
    gb ← produceGamete b
    return $ build name (ga, gb)
