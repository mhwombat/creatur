------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Database.FileSystem
-- Copyright   :  (c) Amy de Buitléir 2012-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A ridiculously simple database that stores each record in a
-- separate file. The name of the file is the record's key.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module ALife.Creatur.Database.FileSystem
  (
    FSDatabase,
    mkFSDatabase
  ) where

import Prelude hiding (readFile, writeFile)

import ALife.Creatur.Database (Database(..), DBRecord, Record, 
  delete, key, keys, store)
import ALife.Creatur.Util (modifyLift)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, gets)
import Data.ByteString as BS (readFile, writeFile)
import qualified Data.Serialize as DS 
  (Serialize, decode, encode)
import System.Directory (createDirectoryIfMissing, doesFileExist, 
  getDirectoryContents, renameFile)

-- | A simple database where each record is stored in a separate file, 
--   and the name of the file is the record's key.
data FSDatabase r = FSDatabase
  {
    initialised :: Bool,
    mainDir :: FilePath,
    archiveDir :: FilePath
  } deriving (Show, Eq)

instance Database (FSDatabase r) where
  type DBRecord (FSDatabase r) = r

  keys = keysIn mainDir

  numRecords = fmap length keys
  
  archivedKeys = keysIn archiveDir

  lookup k = k `lookupIn` mainDir

  lookupInArchive k = k `lookupIn` archiveDir

  store r = do
    initIfNeeded
    writeRecord2 mainDir r

  delete name = do
    initIfNeeded
    d1 <-  gets mainDir
    d2 <- gets archiveDir
    let f1 = d1 ++ '/':name
    let f2 = d2 ++ '/':name
    fileExists <- liftIO $ doesFileExist f1
    when fileExists $ liftIO $ renameFile f1 f2

keysIn
  :: (FSDatabase r -> FilePath) -> StateT (FSDatabase r) IO [String]
keysIn x = do
    initIfNeeded
    d <- gets x
    files <- liftIO $ getDirectoryContents d
    return $ filter isRecordFileName files

lookupIn
  :: DS.Serialize r =>
     String
     -> (FSDatabase r -> FilePath)
     -> StateT (FSDatabase r) IO (Either String r)
lookupIn k x = do
    initIfNeeded
    d <- gets x
    let f = d ++ '/':k
    liftIO $ readRecord3 f
  
-- | @'mkFSDatabase' d@ (re)creates the FSDatabase in the
--   directory @d@.
mkFSDatabase :: FilePath -> FSDatabase r
mkFSDatabase d = FSDatabase False d (d ++ "/archive")

initIfNeeded :: StateT (FSDatabase r) IO ()
initIfNeeded = do
  isInitialised <- gets initialised
  unless isInitialised $ modifyLift initialise

initialise :: FSDatabase r -> IO (FSDatabase r)
initialise u = do
  createDirectoryIfMissing True (mainDir u)
  createDirectoryIfMissing True (archiveDir u)
  return u { initialised=True }

-- | Read a record from a file.
readRecord3 :: DS.Serialize r => FilePath -> IO (Either String r)
readRecord3 f = do
  x <- readFile f
  return $ DS.decode x

-- | Write a record to a file.
writeRecord3 :: (Record r, DS.Serialize r) => FilePath -> r -> IO ()
writeRecord3 f a = do
  let x = DS.encode a
  writeFile f x

writeRecord2 :: (Record r, DS.Serialize r) => 
  (FSDatabase r -> FilePath) -> r -> StateT (FSDatabase r) IO ()
writeRecord2 dirGetter r = do
  d <- gets dirGetter
  let f = d ++ '/':key r
  liftIO $ writeRecord3 f r
  -- liftIO $ agentId r ++ " archived to " ++ show f     

isRecordFileName :: String -> Bool
isRecordFileName s =
  s `notElem` [ "archive", ".", ".." ]

