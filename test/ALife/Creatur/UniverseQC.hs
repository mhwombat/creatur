------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.UniverseQC
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module ALife.Creatur.UniverseQC
  (
    test
  ) where

import qualified ALife.Creatur as A
import ALife.Creatur.Database (Record, key)
import ALife.Creatur.Universe
import Control.Monad.State (execStateT, evalStateT, runStateT)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Test.Framework as TF (Test, testGroup)
import Test.HUnit as TH (assertEqual, assertBool)
import Test.Framework.Providers.HUnit (testCase)

data TestAgent = TestAgent { aAgentId :: String, aIsAlive :: Bool }
  deriving (Eq, Show, Generic)

instance Serialize TestAgent

instance A.Agent TestAgent where
  agentId = aAgentId
  isAlive = aIsAlive

instance Record TestAgent where
  key = aAgentId

tryUniverse :: FilePath -> IO ()
tryUniverse dir = do
  let filename = dir ++ "/universe"
  let u = mkSimpleUniverse "wombat" filename 1000 :: SimpleUniverse TestAgent
  (t, u2) <- runStateT currentTime u
  assertEqual "test1" 0 t
  fileExists <- doesFileExist filename
  assertBool "universe created too early" (not fileExists)

  u3 <- execStateT incTime u2
  t2 <- evalStateT currentTime u3
  assertEqual "test2" 1 t2

  -- Re-read the time. Was it updated properly?
  let u4 = mkSimpleUniverse "wombat" filename 1000 :: SimpleUniverse TestAgent
  t3 <- evalStateT currentTime u4
  assertEqual "test3" 1 t3

  u5 <- execStateT incTime u4
  t4 <- evalStateT currentTime u5
  assertEqual "test4" 2 t4

  -- Re-read the counter. Was it updated properly?
  let u6 = mkSimpleUniverse "wombat" filename 1000 :: SimpleUniverse TestAgent
  t5 <- evalStateT currentTime u6
  assertEqual "test5" 2 t5

testUniverse :: IO ()
testUniverse = withSystemTempDirectory "creaturTest.tmp" tryUniverse

test :: TF.Test
test = testGroup "HUnit ALife.Creatur.UniverseQC"
  [
    testCase "universe"
      testUniverse
  ]


