------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Database.FileSystemQC
-- Copyright   :  (c) 2012-2022 Amy de Buitléir
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

import           ALife.Creatur.Database            (Record, key, lookup, store)
import           ALife.Creatur.Database.FileSystem (mkFSDatabase)
import           Control.Monad.State               (evalStateT, execStateT)
import           Data.Serialize                    (Serialize)
import           GHC.Generics                      (Generic)
import           Prelude                           hiding (lookup)
import           System.IO.Temp                    (withSystemTempDirectory)
import           Test.Framework                    as TF (Test, testGroup)
import           Test.Framework.Providers.HUnit    (testCase)
import           Test.HUnit                        as TH (assertEqual)

data TestRecord = TestRecord String Int
  deriving (Read, Show, Eq, Generic)

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


