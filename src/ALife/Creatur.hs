------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur
-- Copyright   :  (c) Amy de Buitléir 2012-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions used throughout the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}

module ALife.Creatur
 (
    Agent(..),
    AgentId,
    Time,
    programVersion
 ) where

import ALife.Creatur.Database (Record, key)
import Data.Version (showVersion)
import Paths_creatur (version)

-- | The internal clock used by Créatúr is a simple counter.
type Time = Int

-- | A unique ID associated with an agent.
type AgentId = String

-- | An artificial life species.
--   All species used in Créatúr must be an instance of this class.
class (Record a) => Agent a where
  -- | Returns the agent ID.
  agentId :: a -> AgentId
  agentId = key
  -- | Returns True if the agent is alive, false otherwise.
  isAlive :: a -> Bool

programVersion :: String
programVersion = "creatur-" ++ showVersion version
