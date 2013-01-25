------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Universe
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- TODO: fill in
--
------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax, TemplateHaskell, TypeFamilies, 
    FlexibleContexts #-}

module ALife.Creatur.Universe
 (
    Universe(..),
    SimpleUniverse,
    mkSimpleUniverse,
    agentDB,
    clock,
    logger,
    multiLookup,
    extra,
    agentIds,
    addAgent,
    storeOrArchive
 ) where

import ALife.Creatur (Agent, AgentId, agentId, isAlive)
import ALife.Creatur.AgentNamer (AgentNamer, SimpleAgentNamer, 
  mkSimpleAgentNamer)
import qualified ALife.Creatur.AgentNamer as N (genName)
import ALife.Creatur.Clock (Clock, currentTime, incTime)
import ALife.Creatur.Counter (PersistentCounter, mkPersistentCounter)
import ALife.Creatur.Database as D (Database, DBRecord, Record,
  delete, keys, lookup, store)
import ALife.Creatur.Database.FileSystem (FSDatabase, mkFSDatabase)
import ALife.Creatur.Logger (Logger, SimpleRotatingLogger, 
  mkSimpleRotatingLogger, writeToLog)
import Control.Lens (makeLenses, zoom)
import Control.Monad (unless)
import Control.Monad.State (StateT)
import Data.Either (partitionEithers)
import Data.Serialize (Serialize)

-- | A habitat containing artificial life.
data Universe c l d n x a = Universe {
    _clock ∷ c,
    _logger ∷ l,
    _agentDB ∷ d,
    _namer ∷ n,
    _extra ∷ x
  }

makeLenses ''Universe

instance (Clock c, Logger l) ⇒ Logger (Universe c l d n x a) where
  writeToLog msg = do
    t ← currentTime
    zoom logger $ writeToLog $ show t ++ "\t" ++ msg

instance Clock c ⇒ Clock (Universe c l d n x a) where
  currentTime = zoom clock currentTime
  incTime = zoom clock incTime

instance AgentNamer n ⇒ AgentNamer (Universe c l d n x a) where
  genName = zoom namer N.genName

agentIds ∷ Database d ⇒ StateT (Universe c l d n x a) IO [String]
agentIds = zoom agentDB keys

multiLookup ∷ (Serialize a, Database d, Record a, a ~ DBRecord d) ⇒ 
  [AgentId] → StateT d IO (Either String [DBRecord d])
multiLookup names = do
  results ← mapM D.lookup names
  let (msgs, agents) = partitionEithers results
  if null msgs
    then return $ Right agents
    else return . Left . show $ msgs

storeOrArchive ∷ 
  (Serialize a, Database d, Record a, Agent a, a ~ DBRecord d) ⇒
    a → StateT d IO ()
storeOrArchive a = do
  store a -- Even dead agents should be stored (prior to archiving)
  unless (isAlive a) $ (delete . agentId) a

addAgent ∷ (Serialize a, Database d, Record a, a ~ DBRecord d) ⇒
     DBRecord d → StateT (Universe c l d n x a) IO ()
addAgent a = zoom agentDB (store a)

type SimpleUniverse a = 
  Universe PersistentCounter SimpleRotatingLogger (FSDatabase a)
    SimpleAgentNamer () a

mkSimpleUniverse ∷ String → FilePath → Int → SimpleUniverse a
mkSimpleUniverse name dir rotateCount = Universe c l d n ()
  where c = mkPersistentCounter (dir ++ "/clock")
        l = mkSimpleRotatingLogger (dir ++ "/log/") name rotateCount
        d = mkFSDatabase (dir ++ "/db")
        n = mkSimpleAgentNamer (name ++ "_") (dir ++ "/namer")

