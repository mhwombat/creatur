------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Database.CachedFileSystemInternal
-- Copyright   :  (c) Amy de Buitléir 2014-2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A module containing private CachedFileSystem internals.
-- Most developers should use CachedFileSystem instead.
-- This module is subject to change without notice.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
module ALife.Creatur.Database.CachedFileSystemInternal where

import Prelude hiding (readFile, writeFile, lookup)

import ALife.Creatur.Database (Database(..), DBRecord, Record,
  SizedRecord, delete, key, keys, store, size)
import qualified ALife.Creatur.Database.FileSystem as FS
import ALife.Creatur.Util (stateMap)
import Control.Monad (when)
import Control.Monad.State (StateT, get, gets, modify)

-- | A simple database where each record is stored in a separate file, 
--   and the name of the file is the record's key.
data CachedFSDatabase r = CachedFSDatabase
  {
    database :: FS.FSDatabase r,
    cache :: [r],
    maxCacheSize :: Int
  } deriving (Show, Eq)

instance (SizedRecord r) => Database (CachedFSDatabase r) where
  type DBRecord (CachedFSDatabase r) = r

  keys = withFSDB keys

  numRecords = withFSDB numRecords
  
  archivedKeys = withFSDB archivedKeys

  lookup k = do
    x <- fromCache k
    case x of
      Just r  -> return $ Right r
      Nothing -> do
        y <- withFSDB (lookup k)
        case y of
          Right r -> do
            addToCache r
            return $ Right r
          Left s  -> return $ Left s

  lookupInArchive k = withFSDB (lookupInArchive k)
  -- only the main dir is cached

  store r = do
    addToCache r
    withFSDB (store r :: StateT (FS.FSDatabase r) IO ())

  delete k = do
    deleteByKeyFromCache k
    withFSDB (delete k)

withFSDB 
  :: Monad m
    => StateT (FS.FSDatabase r) m a -> StateT (CachedFSDatabase r) m a
withFSDB f = do
  d <- get
  stateMap (\x -> d{database=x}) database f

fromCache :: Record r => String -> StateT (CachedFSDatabase r) IO (Maybe r)
fromCache k = do
  c <- gets cache
  let rs = filter (\r -> key r == k) c
  return $ if null rs
             then Nothing
             else Just (head rs)

addToCache :: SizedRecord r => r -> StateT (CachedFSDatabase r) IO ()
addToCache r = do
  deleteFromCache r
  modify (\d -> d {cache=r:cache d})
  trimCache

deleteByKeyFromCache
  :: SizedRecord r
    => String -> StateT (CachedFSDatabase r) IO ()
deleteByKeyFromCache k
  = modify (\d -> d {cache=filter (\x -> key x /= k) (cache d)})

deleteFromCache
  :: SizedRecord r
    => r -> StateT (CachedFSDatabase r) IO ()
deleteFromCache r =
  modify (\d -> d {cache=filter (\x -> key x /= key r) (cache d)})

trimCache :: SizedRecord r => StateT (CachedFSDatabase r) IO ()
trimCache = do
  m <- gets maxCacheSize
  xs <- gets cache
  when (listSize xs > m) $
    modify (\d -> d {cache=trim m xs})

trim :: SizedRecord r => Int -> [r] -> [r]
trim m xs = if listSize xs > m 
              then trim m (init xs)
              else xs

listSize :: SizedRecord r => [r] -> Int
listSize [] = 0
listSize xs = sum $ map size xs

-- | @'mkFSDatabase' d@ (re)creates the FSDatabase in the
--   directory @d@.
mkCachedFSDatabase :: FilePath -> Int -> CachedFSDatabase r
mkCachedFSDatabase d s = CachedFSDatabase (FS.mkFSDatabase d) [] s
