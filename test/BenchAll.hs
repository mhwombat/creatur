------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Amy de Buitléir 2014-2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Runs the benchmarks.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur.Genetics.BRGCBoolBench (benchmark)
import ALife.Creatur.Genetics.BRGCWord8Bench (benchmark)

import Criterion.Main (Benchmark, defaultMain)

benches :: [Benchmark]
benches = 
  [ 
    ALife.Creatur.Genetics.BRGCBoolBench.benchmark,
    ALife.Creatur.Genetics.BRGCWord8Bench.benchmark
  ]

main :: IO ()
main = defaultMain benches
