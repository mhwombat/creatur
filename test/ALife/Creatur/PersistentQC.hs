------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.PersistentQC
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.PersistentQC
  (
    test
  ) where

import ALife.Creatur.Persistent
import Control.Monad.State (execStateT, evalStateT, runStateT)
import System.IO.Temp (withSystemTempDirectory)
import Test.Framework as TF (Test, testGroup)
import Test.HUnit as TH (assertEqual, assertBool)
import Test.Framework.Providers.HUnit (testCase)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath (dropFileName)

tryPersistent :: FilePath -> IO ()
tryPersistent dir = do
  let filename = dir ++ "/wombat/counter"
  let defaultValue = 'z'
  let k = mkPersistent defaultValue filename
  (v0, k2) <- runStateT getPS k
  assertEqual "test1" defaultValue v0
  dirExists <- doesDirectoryExist $ dropFileName filename
  assertBool "directory created too early" (not dirExists)
  fileExists <- doesFileExist filename
  assertBool "file created too early" (not fileExists)

  let v1 = 'a'
  k3 <- execStateT (putPS v1) k2
  v1a <- evalStateT getPS k3
  assertEqual "test2" v1a v1

  -- Re-read the counter. Was it updated properly?
  let k4 = mkPersistent defaultValue filename
  v1b <- evalStateT getPS k4
  assertEqual "test3" v1b v1

testPersistent :: IO ()
testPersistent = withSystemTempDirectory "creaturTest.tmp" tryPersistent

test :: TF.Test
test = testGroup "HUnit ALife.Creatur.PersistentQC"
  [
    testCase "persistent"
      testPersistent
  ]


