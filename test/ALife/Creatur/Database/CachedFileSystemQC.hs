------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Database.CachedFileSystemQC
-- Copyright   :  (c) Amy de Buitléir 2014-2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module ALife.Creatur.Database.CachedFileSystemQC
  (
    test
  ) where

import Prelude hiding (lookup)
import ALife.Creatur.Database (Record, SizedRecord, key, store, lookup,
  delete, size)
import ALife.Creatur.Database.CachedFileSystemInternal
import Control.Monad.State (execStateT, evalStateT)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.IO.Temp (withSystemTempDirectory)
import Test.Framework as TF (Test, testGroup)
import Test.HUnit as TH (assertEqual, assertBool)
import Test.Framework.Providers.HUnit (testCase)

data TestRecord = TestRecord String Int
  deriving (Show, Eq, Generic)

instance Record TestRecord where
  key (TestRecord k _) = k

instance SizedRecord TestRecord where
  size (TestRecord _ s) = s

instance Serialize TestRecord

tryCachedLookup :: FilePath -> IO ()
tryCachedLookup dir = do
  let record = TestRecord "alpha" 7
  let db = (mkCachedFSDatabase dir 10) { cache=[record] }
  -- We didn't create a file for the record, but as long as the database
  -- finds the record in the cache, this should work.
  Right record' <- evalStateT (lookup (key record)) db
  assertEqual "wombat" record record'

testCachedLookup :: IO ()
testCachedLookup = withSystemTempDirectory "creaturTest.tmp" tryCachedLookup

tryStoreDelete :: FilePath -> IO ()
tryStoreDelete dir = do
  let db = mkCachedFSDatabase dir 10
  let record = TestRecord "alpha" 7
  db' <- execStateT (store record) db
  assertBool "record not in cache" $ record `elem` (cache db')
  db2 <- execStateT (delete $ key record) db'
  assertBool "record still in cache" $ record `notElem` (cache db2)

testStoreDelete :: IO ()
testStoreDelete = withSystemTempDirectory "creaturTest.tmp" tryStoreDelete

tryCachedSize :: FilePath -> IO ()
tryCachedSize dir = do
  let records = map (\n -> TestRecord (show n) 1) ([1..5] :: [Int])
  let db = (mkCachedFSDatabase dir 5) { cache=records }
  let record = TestRecord "one too many" 1
  db' <- execStateT (store record) db
  assertEqual "cache too big" (length (cache db')) 5
  assertBool "record not in cache" $ record `elem` (cache db')

testCachedSize :: IO ()
testCachedSize = withSystemTempDirectory "creaturTest.tmp" tryCachedSize

test :: TF.Test
test = testGroup "HUnit ALife.Creatur.Database.CachedFileSystemQC"
  [
    testCase "cached lookup"
      testCachedLookup,
    testCase "store and delete"
      testStoreDelete,
    testCase "cache size"
      testCachedSize
  ]


