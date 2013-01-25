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

module ALife.Creatur.Genetics.CodeInternal where

import ALife.Creatur.Util (ilogBase, isPowerOf, reverseLookup)
import Codec.Gray (grayCodes)
import Prelude hiding (cycle)

-- | An encoding scheme.
data Code a b = Code { cSize ∷ Int, cTable ∷ [(a,[b])] } deriving Show

-- | Encodes a value as a sequence of bits.
encode ∷ Eq a ⇒ Code a b → a → Maybe [b]
encode = flip lookup . cTable

---- | Given a list of encoding schemes paired with genes, encode all of the
----   genes. Unencodable genes will be skipped.
--encodeAll ∷ Eq a ⇒ [(Code a, a)] → [b]
--encodeAll ps = foldr encodeNext [] ps

encodeNext ∷ Eq a ⇒ (Code a b, a) → [b] → [b]
encodeNext (c, a) bs = maybe bs (bs ++) (encode c a)

-- | Returns the value corresponding to a sequence of bits.
decode ∷ Eq b ⇒ Code a b → [b] → Maybe a
decode = flip reverseLookup . cTable

--decodeAll _ [] = []
--decodeAll bs (c:cs) = g:gs'
--  where g = decode c bs1
--        (bs1, bs2) = splitAt (cSize c) bs
--        gs' = decodeAll bs2 cs

decodeNext ∷ Eq b ⇒ Code a b → [b] → (Maybe a, [b])
decodeNext c bs = (decode c bs1, bs2)
  where (bs1, bs2) = splitAt (cSize c) bs

-- | Convert a list of bits to a string of @0@s and @1@s.
asBits ∷ [Bool] → String
asBits = map (\b → if b then '1' else '0')

-- | Constructs a Gray code for the specified values, using the minimum number
--   of bits required to encode all of the values.
--
--   If the number of values provided is not a perfect square, some codes will
--   not be used; calling @decode@ with those values will return @Nothing@.
--   You can find out if this will be the case by calling @'excessGrayCodes'@.
--   For example @mkGrayCode [\'a\',\'b\',\'c\']@ would assign the code
--   @00@ to @'a'@, @01@ to @'b'@, and @11@ to @'c'@, leaving @10@ unassigned.
--   To avoid having unassigned codes, you can repeat a value in the input 
--   list so the example above could be modified to  
--   @mkGrayCode [\'a\',\'a\',\'b\',\'c\']@, which would assign the codes
--   @00@ and @01@ to 'a', @11@ to @'b'@, and @10@ to @'c'@.
--
--   A Gray code maps values to codes in a way that guarantees that the codes
--   for two consecutive values will differ by only one bit. This feature
--   can be useful in evolutionary programming because the genes resulting 
--   from a crossover operation will be similar to the inputs. This helps to
--   ensure that offspring are similar to their parents, as any radical
--   changes from one generation to the next are the result of mutation
--   alone.
mkGrayCode ∷ [a] → Code a Bool
mkGrayCode xs = Code k (zip xs cs)
  where n = grayCodeLength $ length xs
        k = (length . head) cs
        cs = grayCodes n

-- | @'grayCodeLength' n@ returns the number of bits required to encode @n@
--   values.
grayCodeLength ∷ Int → Int
grayCodeLength n = if n `isPowerOf` 2 then k else k + 1
  where k = ilogBase (2 ∷ Int) n

-- | @'grayCodeCapacity' n@ returns the number of values that can be encoded
--   using @n@ bits. The number of values that can be encoded in n bits is 
--   2^n.
grayCodeCapacity ∷ Int → Int
grayCodeCapacity n = 2^n


