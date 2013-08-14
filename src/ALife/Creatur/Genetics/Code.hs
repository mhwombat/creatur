------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.Code
-- Copyright   :  (c) Amy de Buitléir 2011-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Lookup table for encoding genes.
--
------------------------------------------------------------------------
module ALife.Creatur.Genetics.Code
  (
    -- * Coding schemes
    Code(..),
    -- * Encoding and decoding
    encode,
    encodeNext,
    decode,
    decodeNext
  ) where

import ALife.Creatur.Util (reverseLookup)

-- | An encoding scheme.
data Code a b = Code { cSize :: Int, cTable :: [(a,[b])] } deriving Show

-- | Encodes a value as a sequence of letters in the code alphabet.
encode :: Eq a => Code a b -> a -> Maybe [b]
encode = flip lookup . cTable

-- | Encodes a value and append it to the sequence provided. If the
--   value cannot be encoded, the sequence is returned unmodified.
encodeNext :: Eq a => Code a b -> a -> [b] -> [b]
encodeNext c a bs = maybe bs (bs ++) (encode c a)

-- | Returns the value corresponding to a sequence of letters in the
--   code alphabet.
decode :: Eq b => Code a b -> [b] -> Maybe a
decode = flip reverseLookup . cTable

-- | Decodes a value from a sequence, and returns the value and the
--   unused portion of the sequence.
decodeNext :: Eq b => Code a b -> [b] -> Maybe (a, [b])
decodeNext c bs = decode c bs1 >>= \g -> Just (g, bs2)
  where (bs1, bs2) = splitAt (cSize c) bs
