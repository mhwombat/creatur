------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.Gene
-- Copyright   :  (c) Amy de Buitléir 2011-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions related to artificial genes.
--
------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax #-}

module ALife.Creatur.Genetics.Gene 
  (
--    ACTG(..),
    PairedGene(..),
    decodeAndExpress
  ) where

import ALife.Creatur.Genetics.Code (Code, decodeNext)

-- | A paired instruction for building an agent.
class PairedGene g where
  -- | Given two possible forms of a PairedGene, @'express'@ takes into
  --   account any dominance relationship, and returns a PairedGene
  --   representing the result.
  express ∷ g → g → g

-- | Read the next pair of PairedGenes from a two sequences of 
--   "nucleotides", and return the resulting PairedGene (after taking
--   into account any dominance relationship) and the remaining (unread)
--   portion of the two nucleotide strands.
decodeAndExpress ∷ 
  (PairedGene g, Eq n) ⇒ Code g n → ([n], [n]) → (Maybe g, ([n], [n]))
decodeAndExpress c (as,bs) = (expressMaybe a b, (as',bs'))
  where (a,as') = decodeNext c as
        (b,bs') = decodeNext c bs

expressMaybe ∷ PairedGene g ⇒ Maybe g → Maybe g → Maybe g
expressMaybe (Just a) (Just b) = Just (express a b)
expressMaybe (Just a) Nothing  = Just a
expressMaybe Nothing (Just b)  = Just b
expressMaybe Nothing Nothing   = Nothing


