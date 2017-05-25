------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Database.CachedFileSystem
-- Copyright   :  (c) Amy de Buitléir 2014-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A database that stores each record in a separate file and maintains
-- a cache of recently-accessed records. The name of the file is the
-- record's key.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module ALife.Creatur.Database.CachedFileSystem
  (
    CachedFSDatabase,
    mkCachedFSDatabase
  ) where

import ALife.Creatur.Database.CachedFileSystemInternal
