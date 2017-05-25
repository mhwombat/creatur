------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.CounterQC
-- Copyright   :  (c) Amy de Buitléir 2012-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module ALife.Creatur.CounterQC
  (
    test
  ) where

import ALife.Creatur.Counter
import Control.Monad.State (execStateT, evalStateT, runStateT)
import System.IO.Temp (withSystemTempDirectory)
import Test.Framework as TF (Test, testGroup)
import Test.HUnit as TH (assertEqual, assertBool)
import Test.Framework.Providers.HUnit (testCase)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (dropFileName)

tryCounter :: FilePath -> IO ()
tryCounter dir = do
  let filename = dir ++ "/wombat/counter"
  let k = mkPersistentCounter filename
  (initialCount, k2) <- runStateT current k
  assertEqual "test1" initialCount 0
  dirExists <- doesDirectoryExist $ dropFileName filename
  assertBool "directory created too early" (not dirExists)
  fileExists <- doesFileExist filename
  assertBool "file created too early" (not fileExists)

  k3 <- execStateT increment k2
  nextCount <- evalStateT current k3
  assertEqual "test2" 1 nextCount

  -- Re-read the counter. Was it updated properly?
  let k4 = mkPersistentCounter filename
  oldCount <- evalStateT current k4
  assertEqual "test3" 1 oldCount

  k5 <- execStateT increment k4
  nextCount2 <- evalStateT current k5
  assertEqual "test4" 2 nextCount2

  -- Re-read the counter. Was it updated properly?
  let k6 = mkPersistentCounter filename
  oldCount2 <- evalStateT current k6
  assertEqual "test5" 2 oldCount2

testCounter :: IO ()
testCounter = withSystemTempDirectory "creaturTest.tmp" tryCounter

test :: TF.Test
test = testGroup "HUnit ALife.Creatur.CounterQC"
  [
    testCase "counter"
      testCounter
  ]


