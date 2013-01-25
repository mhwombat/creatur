{-# LANGUAGE UnicodeSyntax, TypeFamilies, FlexibleContexts #-}

-- | Represents r FSDatabase (including agents, clock, logging facility,
--   etc.) that can run within the Créatúr framework.
module ALife.Creatur.Database.FileSystem
  (
    FSDatabase,
    mkFSDatabase
  ) where

import ALife.Creatur.Database (Database(..), DBRecord, Record, 
  delete, key, keys, store)
import Prelude hiding (readFile, writeFile)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, gets, put)
import Data.ByteString as BS (readFile, writeFile)
import qualified Data.Serialize as DS 
  (Serialize, decode, encode)
import System.Directory (createDirectoryIfMissing, doesFileExist, 
  getDirectoryContents, removeFile)

-- | A simple database where each record is stored in a separate file, and
--   the name of the file is the record's key.
data FSDatabase r = FSDatabase
  {
    initialised ∷ Bool,
    mainDir ∷ FilePath,
    archiveDir ∷ FilePath
  } deriving Show

instance Database (FSDatabase r) where
  type DBRecord (FSDatabase r) = r

  keys = do
    initIfNeeded
    d ← gets mainDir
    files ← liftIO $ getDirectoryContents d
    return $ filter isRecordFileName files

  lookup k = do
    initIfNeeded
    d ← gets mainDir
    let f = d ++ '/':k
    liftIO $ readRecord3 f

  store r = do
    initIfNeeded
    writeRecord2 mainDir r

  delete name = do
    initIfNeeded
    fileExists ← liftIO $ doesFileExist name
    when fileExists $ liftIO $ removeFile name

-- | @'mkFSDatabase' d@ (re)creates the FSDatabase in the
--   directory @d@.
mkFSDatabase ∷ FilePath → FSDatabase r
mkFSDatabase d = FSDatabase False d (d ++ "/archive")

initIfNeeded ∷ StateT (FSDatabase r) IO ()
initIfNeeded = do
  isInitialised ← gets initialised
  unless isInitialised $ do
    u ← get
    u' ← liftIO $ initialise u
    put u'

initialise ∷ FSDatabase r → IO (FSDatabase r)
initialise u = do
  createDirectoryIfMissing True (mainDir u)
  createDirectoryIfMissing True (archiveDir u)
  return u { initialised=True }

-- | Read a record from a file.
readRecord3 ∷ DS.Serialize r ⇒ FilePath → IO (Either String r)
readRecord3 f = do
  x ← readFile f
  return $ DS.decode x

-- | Write a record to a file.
writeRecord3 ∷ (Record r, DS.Serialize r) ⇒ FilePath → r → IO ()
writeRecord3 f a = do
  let x = DS.encode a
  writeFile f x

writeRecord2 ∷ (Record r, DS.Serialize r) ⇒ 
  (FSDatabase r → FilePath) → r → StateT (FSDatabase r) IO ()
writeRecord2 dirGetter r = do
  d ← gets dirGetter
  let f = d ++ '/':key r
  liftIO $ writeRecord3 f r
  -- liftIO $ agentId r ++ " archived to " ++ show f     

isRecordFileName ∷ String → Bool
isRecordFileName s =
  s `notElem` [ "archive", ".", ".." ]

