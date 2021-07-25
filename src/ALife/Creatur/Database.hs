------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Database
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Database interface for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, ConstrainedClassMethods #-}

module ALife.Creatur.Database
 (
    Database(..),
    Record(..),
    SizedRecord(..)
 ) where

import Prelude hiding (lookup)
import Control.Monad.State (StateT)
import Data.Serialize (Serialize)

class Record r where
  key :: r -> String

class (Record r) => SizedRecord r where
  size :: r -> Int

-- | A database offering storage and retrieval for records.
class Database d where
  type DBRecord d
  -- | Get a list of all active keys in the database.
  keys :: StateT d IO [String]
  -- | Return the number of records stored in the database.
  numRecords :: StateT d IO Int
  -- | Get a list of all archived keys in the database. If the database
  --   does not implement archiving, it may return an empty list.
  archivedKeys :: StateT d IO [String]
  -- | Read an active record from the database.
  lookup :: Serialize (DBRecord d) => 
    String -> StateT d IO (Either String (DBRecord d))
  -- | Read an archived record from the database.
  lookupInArchive :: Serialize (DBRecord d) => 
    String -> StateT d IO (Either String (DBRecord d))
  -- | Fetch all active records from the database.
  readAll :: Serialize (DBRecord d) => 
    StateT d IO [Either String (DBRecord d)]
  readAll = keys >>= mapM lookup
  -- | Read all archived records from the database.
  readAllInArchive :: Serialize (DBRecord d) => 
    StateT d IO [Either String (DBRecord d)]
  readAllInArchive = archivedKeys >>= mapM lookupInArchive
  -- | Write a record to the database. 
  --   If an agent with the same name already exists, it will be overwritten.
  store :: (Record (DBRecord d), Serialize (DBRecord d)) => 
    DBRecord d -> StateT d IO ()
  -- | Remove a record from the database.
  --   The database may archive records rather than simply deleting them.
  delete :: Serialize (DBRecord d) => String -> StateT d IO ()

