------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Universe
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides a habitat for artificial life.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
module ALife.Creatur.Universe
  (
    -- * Constructors
    Universe(..),
    SimpleUniverse,
    mkSimpleUniverse,
    -- * Clock
    currentTime,
    incTime,
    -- * Logging
    writeToLog,
    -- * Database
    agentIds,
    archivedAgentIds,
    getAgent,
    getAgentFromArchive,
    getAgents,
    addAgent,
    storeOrArchive,
    -- * Names
    genName,
    -- * Agent programs
    AgentProgram,
    withAgent,
    AgentsProgram,
    withAgents,
    -- * Agent rotation
    lineup,
    endOfRound,
    refresh,
    rotate
 ) where

import Prelude hiding (lookup)

import qualified ALife.Creatur as A
import qualified ALife.Creatur.AgentNamer as N
import qualified ALife.Creatur.Checklist as CL
import qualified ALife.Creatur.Clock as C
import qualified ALife.Creatur.Counter as K
import qualified ALife.Creatur.Database as D
import qualified ALife.Creatur.Database.FileSystem as FS
import qualified ALife.Creatur.Logger as L
import ALife.Creatur.Util (stateMap, shuffle)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.State (StateT, get)
import Data.Either (partitionEithers)
import Data.Serialize (Serialize)

-- | A habitat containing artificial life.
class (C.Clock (Clock u), L.Logger (Logger u), D.Database (AgentDB u),
  N.AgentNamer (AgentNamer u), CL.Checklist (Checklist u),
  A.Agent (Agent u), D.Record (Agent u),
  Agent u ~ D.DBRecord (AgentDB u))
      => Universe u where
  type Agent u
  type Clock u
  clock :: u -> Clock u
  setClock :: u -> Clock u -> u
  type Logger u
  logger :: u -> Logger u
  setLogger :: u -> Logger u -> u
  type AgentDB u
  agentDB :: u -> AgentDB u
  setAgentDB :: u -> AgentDB u -> u
  type AgentNamer u
  agentNamer :: u -> AgentNamer u
  setAgentNamer :: u -> AgentNamer u -> u
  type Checklist u
  checklist :: u -> Checklist u
  setChecklist :: u -> Checklist u -> u

withClock :: (Universe u, Monad m) => StateT (Clock u) m a -> StateT u m a
withClock program = do
  u <- get
  stateMap (setClock u) clock program

withLogger
  :: (Universe u, Monad m)
    => StateT (Logger u) m a -> StateT u m a
withLogger program = do
  u <- get
  stateMap (setLogger u) logger program

withAgentDB
  :: (Universe u, Monad m)
    => StateT (AgentDB u) m a -> StateT u m a
withAgentDB program = do
  u <- get
  stateMap (setAgentDB u) agentDB program

withAgentNamer
  :: (Universe u, Monad m)
    => StateT (AgentNamer u) m a -> StateT u m a
withAgentNamer program = do
  u <- get
  stateMap (setAgentNamer u) agentNamer program

withChecklist
  :: (Universe u, Monad m)
    => StateT (Checklist u) m a -> StateT u m a
withChecklist program = do
  u <- get
  stateMap (setChecklist u) checklist program

currentTime :: Universe u => StateT u IO A.Time
currentTime = withClock C.currentTime

incTime :: Universe u => StateT u IO ()
incTime = withClock C.incTime

writeToLog :: Universe u => String -> StateT u IO ()
writeToLog msg = do
  t <- currentTime
  let logMsg = show t ++ "\t" ++ msg
  withLogger $ L.writeToLog logMsg

genName :: Universe u => StateT u IO A.AgentId
genName = withAgentNamer N.genName

agentIds :: Universe u => StateT u IO [A.AgentId]
agentIds = withAgentDB D.keys

archivedAgentIds :: Universe u => StateT u IO [A.AgentId]
archivedAgentIds = withAgentDB D.archivedKeys

getAgent
  :: (Universe u, Serialize (Agent u))
    => A.AgentId -> StateT u IO (Either String (Agent u))
getAgent name = withAgentDB (D.lookup name)

getAgentFromArchive
  :: (Universe u, Serialize (Agent u))
    => A.AgentId -> StateT u IO (Either String (Agent u))
getAgentFromArchive name = withAgentDB (D.lookupInArchive name)

getAgents
  :: (Universe u, Serialize (Agent u))
    => [A.AgentId] -> StateT u IO (Either String [Agent u])
getAgents names = do
  selected <- mapM getAgent names
  let (msgs, agents) = partitionEithers selected
  if null msgs
    then return $ Right agents
    else return . Left $ show msgs

storeOrArchive
  :: (Universe u, Serialize (Agent u))
    => Agent u -> StateT u IO ()
storeOrArchive a = do
  withAgentDB (D.store a) -- Even dead agents should be stored (prior to archiving)
  if A.isAlive a
    then writeToLog $ (A.agentId a) ++ " returned to population"
    else do
      withAgentDB (D.delete $ A.agentId a)
      writeToLog $ (A.agentId a) ++ " has been archived"

addAgent
  :: (Universe u, Serialize (Agent u))
    => Agent u -> StateT u IO ()
addAgent a = withAgentDB $ D.store a

-- | A program involving one agent.
--   The input parameter is the agent.
--   The program must return the agent (which may have been modified).
type AgentProgram u = Agent u -> StateT u IO (Agent u)

-- | Run a program involving one agent
withAgent
  :: (Universe u, Serialize (Agent u))
    => AgentProgram u -> A.AgentId -> StateT u IO ()
withAgent program name = do
  result <- getAgent name
  case result of
    Left msg ->
      writeToLog $ "Unable to read '" ++ name ++ "': " ++ msg
    Right a ->
      program a >>= storeOrArchive

-- | A program involving multiple agents.
--   The input parameter is a list of agents.
--   The program must return a list of agents that have been *modified*.
--   The order of the output list is not important.
type AgentsProgram u = [Agent u] -> StateT u IO [Agent u]

-- Run a program involving multiple agents.
withAgents
  :: (Universe u, Serialize (Agent u))
    => AgentsProgram u -> [A.AgentId] -> StateT u IO ()
withAgents program names = do
  result <- getAgents names
  case result of
    Left msg ->
      writeToLog $ "Unable to read '" ++ show names ++ "': " ++ msg
    Right as ->
      program as >>= mapM_ storeOrArchive

lineup :: Universe u => StateT u IO [A.AgentId]
lineup = do
  (xs,ys) <- withChecklist CL.status
  return $ xs ++ ys

endOfRound :: Universe u => StateT u IO Bool
endOfRound = withChecklist CL.done

refresh :: Universe u => StateT u IO ()
refresh = do
  as <- shuffledAgentIds
  withChecklist (CL.setItems as)

rotate :: Universe u => StateT u IO ()
rotate = withChecklist CL.markDone

shuffledAgentIds :: Universe u => StateT u IO [String]
shuffledAgentIds
  = agentIds >>= liftIO . evalRandIO . shuffle

data SimpleUniverse a = SimpleUniverse
  {
    suClock :: K.PersistentCounter,
    suLogger :: L.SimpleRotatingLogger,
    suDB :: (FS.FSDatabase a),
    suNamer :: N.SimpleAgentNamer,
    suChecklist :: CL.PersistentChecklist
  }

instance (A.Agent a, Serialize a) => Universe (SimpleUniverse a) where
  type Agent (SimpleUniverse a) = a
  type Clock (SimpleUniverse a) = K.PersistentCounter
  clock = suClock
  setClock u c = u { suClock=c }
  type Logger (SimpleUniverse a) = L.SimpleRotatingLogger
  logger = suLogger
  setLogger u l = u { suLogger=l }
  type AgentDB (SimpleUniverse a) = FS.FSDatabase a
  agentDB = suDB
  setAgentDB u d = u { suDB=d }
  type AgentNamer (SimpleUniverse a) = N.SimpleAgentNamer
  agentNamer = suNamer
  setAgentNamer u n = u { suNamer=n }
  type Checklist (SimpleUniverse a) = CL.PersistentChecklist
  checklist = suChecklist
  setChecklist u cl = u { suChecklist=cl }

mkSimpleUniverse :: String -> FilePath -> Int -> SimpleUniverse a
mkSimpleUniverse name dir rotateCount
  = SimpleUniverse c l d n cl
  where c = K.mkPersistentCounter (dir ++ "/clock")
        l = L.mkSimpleRotatingLogger (dir ++ "/log/") name rotateCount
        d = FS.mkFSDatabase (dir ++ "/db")
        n = N.mkSimpleAgentNamer (name ++ "_") (dir ++ "/namer")
        cl = CL.mkPersistentChecklist (dir ++ "/todo")
