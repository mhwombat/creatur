------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Database
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Database interface for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module ALife.Creatur.Database
 (
    Database(..),
    Record(..)
 ) where

import Control.Monad.State (StateT)
import Data.Serialize (Serialize)

class Record r where
  key :: r -> String
  
-- | A database offering storage and retrieval for records.
class Database d where
  type DBRecord d
  -- | Get a list of all keys in the database.
  keys :: StateT d IO [String]
  -- | Read a record from the database.
  lookup :: Serialize (DBRecord d) => 
    String -> StateT d IO (Either String (DBRecord d))
  -- | Write a record to the database. 
  --   If an agent with the same name already exists, it will be overwritten.
  store :: (Record (DBRecord d), Serialize (DBRecord d)) => 
    DBRecord d -> StateT d IO ()
  -- | Remove a record from the database.
  --   The database may archive records rather than simply deleting them.
  delete :: Serialize (DBRecord d) => String -> StateT d IO ()

