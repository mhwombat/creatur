------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Database.FileSystemQC
-- Copyright   :  (c) Amy de Buitléir 2012-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module ALife.Creatur.Database.FileSystemQC
  (
    test
  ) where

import Prelude hiding (lookup)
import ALife.Creatur.Database (Record, key, store, lookup)
import ALife.Creatur.Database.FileSystem (mkFSDatabase)
import Control.Monad.State (execStateT, evalStateT)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.IO.Temp (withSystemTempDirectory)
import Test.Framework as TF (Test, testGroup)
import Test.HUnit as TH (assertEqual)
import Test.Framework.Providers.HUnit (testCase)

data TestRecord = TestRecord String Int
  deriving (Show, Eq, Generic)

instance Record TestRecord where
  key (TestRecord k _) = k

instance Serialize TestRecord

tryStoreLookup :: FilePath -> IO ()
tryStoreLookup dir = do
  let db = mkFSDatabase dir
  let record = TestRecord "alpha" 7
  db' <- execStateT (store record) db
  Right record' <- evalStateT (lookup (key record)) db'
  assertEqual "wombat" record record'

testStoreLookup :: IO ()
testStoreLookup = withSystemTempDirectory "creaturTest.tmp" tryStoreLookup

test :: TF.Test
test = testGroup "HUnit ALife.Creatur.Database.FileSystemQC"
  [
    testCase "store and lookup"
      testStoreLookup
  ]


