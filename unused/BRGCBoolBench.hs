------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.BRGCBoolBench
-- Copyright   :  (c) Amy de Buitléir 2014-2019
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Benchmarks.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module ALife.Creatur.Genetics.BRGCBoolBench
  (
    benchmark
  ) where

import Prelude hiding (read)
import ALife.Creatur.Genetics.BRGCBool
import Data.Word (Word8)
import Criterion.Main (Benchmark, Pure, nf, bgroup, bench)

word8 :: Word8
word8 = 0

word8Encoded :: [Bool]
word8Encoded = write word8
                
putWord8Benchmark :: Pure
putWord8Benchmark = nf write word8

getWord8Benchmark :: Pure
getWord8Benchmark = nf (read :: Sequence -> Either [String] Word8) word8Encoded

word8s :: [Word8]
word8s = [0..255]

word8sEncoded :: [Bool]
word8sEncoded = write word8s

putWord8sBenchmark :: Pure
putWord8sBenchmark = nf write word8s

getWord8sBenchmark :: Pure
getWord8sBenchmark = nf (read :: Sequence -> Either [String] [Word8]) word8sEncoded

benchmark :: Benchmark
benchmark = bgroup "ALife.Creatur.Genetics.BRGCBoolBench"
  [
    bench "put Word8" putWord8Benchmark,
    bench "get Word8" getWord8Benchmark,
    bench "put [Word8]" putWord8sBenchmark,
    bench "get [Word8]" getWord8sBenchmark
  ]


