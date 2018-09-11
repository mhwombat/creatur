------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.ChecklistQC
-- Copyright   :  (c) Amy de Buitléir 2013-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module ALife.Creatur.ChecklistQC
  (
    test
  ) where

import qualified ALife.Creatur as A
import ALife.Creatur.Database (Record, key)
import ALife.Creatur.Checklist
import Control.Monad.State (execStateT, evalStateT)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
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

testChecklist :: IO ()
testChecklist = withSystemTempDirectory "creaturTest.tmp" testChecklist'

testChecklist' :: FilePath -> IO ()
testChecklist' dir = do
  let filename = dir ++ "/checklist"
  let c = mkPersistentChecklist filename
  c1 <- execStateT (setItems ["taskA", "taskB", "taskC"]) c
  s1 <- evalStateT status c1
  assertEqual "test1" (["taskA", "taskB", "taskC"],[]) s1

  c2 <- execStateT (markDone "taskB") c1
  s2 <- evalStateT status c2
  assertEqual "test2" (["taskA", "taskC"],["taskB"]) s2

  c3 <- execStateT (delete "taskA") c2
  s3 <- evalStateT status c3
  assertEqual "test3" (["taskC"],["taskB"]) s3

  c4 <- execStateT (markDone "taskA") c3
  s4 <- evalStateT status c4
  assertEqual "test4" (["taskC"],["taskB"]) s4

  c5 <- execStateT (markDone "taskC") c4
  s5 <- evalStateT status c5
  assertEqual "test5" ([],["taskB", "taskC"]) s5
  d5 <- evalStateT done c5
  assertBool "test5a" d5

test :: TF.Test
test = testGroup "HUnit ALife.Creatur.ChecklistQC"
  [
    testCase "testChecklist" testChecklist
  ]


