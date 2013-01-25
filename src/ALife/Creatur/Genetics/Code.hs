------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.Code
-- Copyright   :  (c) Amy de Buitléir 2011-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Encoding schemes for genes.
--
------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax #-}

module ALife.Creatur.Genetics.Code
  (
    -- * Coding schemes
    Code,
    mkGrayCode,
    -- * Encoding and decoding
    encode,
    encodeNext,
    decode,
    decodeNext,
    -- * Miscellaneous
    asBits
  ) where

import ALife.Creatur.Genetics.CodeInternal (Code, mkGrayCode, encode, 
  encodeNext, decode, decodeNext, asBits)

