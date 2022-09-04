------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Runs the QuickCheck tests.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur.CounterQC (test)
import ALife.Creatur.ChecklistQC (test)
import ALife.Creatur.UniverseQC (test)
import ALife.Creatur.Database.FileSystemQC (test)
import ALife.Creatur.Database.CachedFileSystemQC (test)
import ALife.Creatur.Genetics.DiploidQC (test)
import ALife.Creatur.Genetics.RecombinationQC (test)
import ALife.Creatur.Genetics.BRGCBoolQC (test)
import ALife.Creatur.Genetics.BRGCWord8QC (test)
import ALife.Creatur.Genetics.BRGCWord16QC (test)
import ALife.Creatur.PersistentQC (test)
import ALife.Creatur.UtilQC (test)

import Test.Framework as TF (defaultMain, Test)

tests :: [TF.Test]
tests = 
  [
    -- Tests are in order of increasing complexity
    ALife.Creatur.PersistentQC.test,
    ALife.Creatur.CounterQC.test,
    ALife.Creatur.ChecklistQC.test,
    ALife.Creatur.UniverseQC.test,
    ALife.Creatur.Database.FileSystemQC.test,
    ALife.Creatur.Database.CachedFileSystemQC.test,
    ALife.Creatur.UtilQC.test,
    ALife.Creatur.Genetics.DiploidQC.test,
    ALife.Creatur.Genetics.RecombinationQC.test,
    ALife.Creatur.Genetics.BRGCBoolQC.test,
    ALife.Creatur.Genetics.BRGCWord8QC.test,
    ALife.Creatur.Genetics.BRGCWord16QC.test
  ]

main :: IO ()
main = defaultMain tests
