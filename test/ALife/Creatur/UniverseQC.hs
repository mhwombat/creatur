------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.UniverseQC
-- Copyright   :  (c) Amy de Buitléir 2012-2019
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
import System.Directory (doesFileExist, doesDirectoryExist)
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

testUniverseCreation :: IO ()
testUniverseCreation = withSystemTempDirectory "creaturTest.tmp" testUniverseCreation'

testUniverseCreation' :: FilePath -> IO ()
testUniverseCreation' dir = do
  let logFileName = dir ++ "/universe"
  let u = mkSimpleUniverse "wombat" logFileName :: SimpleUniverse TestAgent
  (t, u2) <- runStateT currentTime u
  assertEqual "test1" 0 t
  dirExists <- doesDirectoryExist logFileName
  assertBool "universe created too early" (not dirExists)

  u3 <- execStateT incTime u2
  t2 <- evalStateT currentTime u3
  assertEqual "test2" 1 t2

  -- Re-read the time. Was it updated properly?
  let u4 = mkSimpleUniverse "wombat" logFileName :: SimpleUniverse TestAgent
  t3 <- evalStateT currentTime u4
  assertEqual "test3" 1 t3

  u5 <- execStateT incTime u4
  t4 <- evalStateT currentTime u5
  assertEqual "test4" 2 t4

  -- Re-read the counter. Was it updated properly?
  let u6 = mkSimpleUniverse "wombat" logFileName :: SimpleUniverse TestAgent
  t5 <- evalStateT currentTime u6
  assertEqual "test5" 2 t5

testUniverseDB :: IO ()
testUniverseDB = withSystemTempDirectory "creaturTest.tmp" testUniverseDB'

testUniverseDB' :: FilePath -> IO ()
testUniverseDB' dir = do
  let logFileName = dir ++ "/universe"
  let u = mkSimpleUniverse "wombat" logFileName :: SimpleUniverse TestAgent
  let a = TestAgent "agent_a" True
  u2 <- execStateT (store a) u
  xs <- evalStateT agentIds u2
  assertBool "agent not in agentIds list" ((A.agentId a) `elem` xs)
  let a2 = a { aIsAlive=False }
  u3 <- execStateT (store a2) u2
  xs' <- evalStateT agentIds u3
  assertBool "agent still in agentIds list" ((A.agentId a) `notElem` xs')
  let f = logFileName ++ "/db/" ++ A.agentId a
  fileExists <- doesFileExist f
  assertBool "agent file not removed" (not fileExists)
  let f2 = logFileName ++ "/db/archive/" ++ A.agentId a
  fileExists2 <- doesFileExist f2
  assertBool "agent file not archived" (fileExists2)

testUniverseLineup :: IO ()
testUniverseLineup = withSystemTempDirectory "creaturTest.tmp" testUniverseLineup'

testUniverseLineup' :: FilePath -> IO ()
testUniverseLineup' dir = do
  let logFileName = dir ++ "/universe"
  let u = mkSimpleUniverse "wombat" logFileName :: SimpleUniverse TestAgent
  let a = TestAgent "agent_a" True
  let b = TestAgent "agent_b" True
  let c = TestAgent "agent_c" True
  u2 <- execStateT (store a >> store b >> store c >> refreshLineup) u
  xs <- evalStateT lineup u2
  assertBool "agent not in lineup" ((A.agentId a) `elem` xs)
  let a2 = a { aIsAlive=False }
  u3 <- execStateT (store a2 >> markDone (A.agentId a2)) u2
  xs' <- evalStateT lineup u3
  assertBool "agent still in lineup" ((A.agentId a) `notElem` xs')

test :: TF.Test
test = testGroup "HUnit ALife.Creatur.UniverseQC"
  [
    testCase "testUniverseCreation" testUniverseCreation,
    testCase "testUniverseDB" testUniverseDB,
    testCase "testUniverseLineup" testUniverseLineup
  ]


