------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Universe
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides a habitat for artificial life.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ALife.Creatur.Universe
  (
    -- * Constructors
    Universe(..),
    SimpleUniverse,
    CachedUniverse,
    mkSimpleUniverse,
    mkCachedUniverse,
    -- * Clock
    currentTime,
    incTime,
    -- * Logging
    writeToLog,
    -- * Database
    agentIds,
    archivedAgentIds,
    popSize,
    getAgent,
    getAgentFromArchive,
    getAgents,
    -- addAgent,
    store,
    -- * Names
    genName,
    -- * Agent programs
    AgentProgram,
    withAgent,
    AgentsProgram,
    withAgents,
    isNew, -- exported for testing only
    -- * Agent rotation
    lineup,
    startOfRound,
    endOfRound,
    refreshLineup,
    markDone
 ) where

import           Prelude                                 hiding (lookup)

import qualified ALife.Creatur                           as A
import qualified ALife.Creatur.Checklist                 as CL
import qualified ALife.Creatur.Clock                     as C
import qualified ALife.Creatur.Counter                   as K
import qualified ALife.Creatur.Database                  as D
import qualified ALife.Creatur.Database.CachedFileSystem as CFS
import qualified ALife.Creatur.Database.FileSystem       as FS
import qualified ALife.Creatur.Logger                    as L
import qualified ALife.Creatur.Logger.SimpleLogger       as SL
import qualified ALife.Creatur.Namer                     as N
import           ALife.Creatur.Util                      (shuffle, stateMap)
import           Control.Exception                       (SomeException)
import           Control.Monad.Catch                     (catchAll)
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Random                    (evalRandIO)
import           Control.Monad.State                     (StateT, get)
import           Data.Either                             (partitionEithers)
import           Data.Serialize                          (Serialize)
import           GHC.Stack                               (callStack,
                                                          prettyCallStack)

-- | A habitat containing artificial life.
class (C.Clock (Clock u), L.Logger (Logger u), D.Database (AgentDB u),
  N.Namer (Namer u), CL.Checklist (Checklist u), A.Agent (Agent u),
  D.Record (Agent u), Agent u ~ D.DBRecord (AgentDB u))
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
  type Namer u
  agentNamer :: u -> Namer u
  setNamer :: u -> Namer u -> u
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

withNamer
  :: (Universe u, Monad m)
    => StateT (Namer u) m a -> StateT u m a
withNamer program = do
  u <- get
  stateMap (setNamer u) agentNamer program

withChecklist
  :: (Universe u, Monad m)
    => StateT (Checklist u) m a -> StateT u m a
withChecklist program = do
  u <- get
  stateMap (setChecklist u) checklist program

-- | The current "time" (counter) in this universe
currentTime :: Universe u => StateT u IO A.Time
currentTime = withClock C.currentTime

-- | Increment the current "time" (counter) in this universe.
incTime :: Universe u => StateT u IO ()
incTime = withClock C.incTime

-- | Write a message to the log file for this universe.
writeToLog :: Universe u => String -> StateT u IO ()
writeToLog msg = do
  t <- currentTime
  let logMsg = show t ++ "\t" ++ msg
  withLogger $ L.writeToLog logMsg

-- | Generate a unique name for a new agent.
genName :: Universe u => StateT u IO A.AgentId
genName = withNamer N.genName

-- | Returns the list of agents in the population.
agentIds :: Universe u => StateT u IO [A.AgentId]
agentIds = withAgentDB D.keys

-- | Returns the list of (dead) agents in the archive.
archivedAgentIds :: Universe u => StateT u IO [A.AgentId]
archivedAgentIds = withAgentDB D.archivedKeys

-- | Returns the current size of the population.
popSize :: Universe u => StateT u IO Int
popSize = withAgentDB D.numRecords

-- | Fetches the agent with the specified ID from the population.
--   Note: Changes made to this agent will not "take" until
--   @'store'@ is called.
getAgent
  :: (Universe u, Serialize (Agent u))
    => A.AgentId -> StateT u IO (Either String (Agent u))
getAgent name = do
  result <- withAgentDB (D.lookup name)
  case result of
    Left msg -> do
      writeToLog $ "Unable to read " ++ name ++ ": " ++ msg
      archive name
    Right _  -> return ()
  return result

-- | Fetches the agent with the specified ID from the archive.
getAgentFromArchive
  :: (Universe u, Serialize (Agent u))
    => A.AgentId -> StateT u IO (Either String (Agent u))
getAgentFromArchive name = withAgentDB (D.lookupInArchive name)

-- | Fetches the agents with the specified IDs from the population.
getAgents
  :: (Universe u, Serialize (Agent u))
    => [A.AgentId] -> StateT u IO [Agent u]
getAgents names = do
  selected <- mapM getAgent names
  let (msgs, agents) = partitionEithers selected
  mapM_ writeToLog msgs
  return agents

-- | If the agent is alive, adds it to the population (replacing the
--   the previous copy of that agent, if any). If the agent is dead,
--   places it in the archive.
store
  :: (Universe u, Serialize (Agent u))
    => Agent u -> StateT u IO ()
store a = do
  newAgent <- isNew (A.agentId a)
  withAgentDB (D.store a) -- Even dead agents should be stored (prior to archiving)
  if A.isAlive a
    then
      if newAgent
         then writeToLog $ A.agentId a ++ " added to population"
         else writeToLog $ A.agentId a ++ " returned to population"
    else archive (A.agentId a)

archive
  :: (Universe u, Serialize (Agent u))
    => A.AgentId -> StateT u IO ()
archive name = do
  withAgentDB $ D.delete name
  withChecklist $ CL.delete name
  writeToLog $ name ++ " archived and removed from lineup"

isNew :: Universe u => A.AgentId -> StateT u IO Bool
isNew name = fmap (name `notElem`) agentIds

-- -- | Adds an agent to the universe.
-- addAgent
--   :: (Universe u, Serialize (Agent u))
--     => Agent u -> StateT u IO ()
-- addAgent a = withAgentDB $ D.store a

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
      catchAll (program a >>= store) (handleException . A.agentId $ a)

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
  as <- getAgents names
  catchAll (program as >>= mapM_ store)
      (handleException . A.agentId  . head $ as)

handleException
  :: (Universe u, Serialize (Agent u))
    => A.AgentId -> SomeException -> StateT u IO ()
handleException a e = do
  writeToLog $ "WARNING: Unhandled exception: " ++ show e
  writeToLog $ "WARNING: Call stack: " ++ prettyCallStack callStack
  archive a

-- | Returns the current lineup of (living) agents in the universe.
--   Note: Check for @'endOfRound'@ and call @'refreshLineup'@ if needed
--   before invoking this function.
lineup :: Universe u => StateT u IO [A.AgentId]
lineup = do
  (xs,ys) <- withChecklist CL.status
  return $ xs ++ ys

-- | Returns true if no agents have yet their turn at the CPU for this
--   round.
startOfRound :: Universe u => StateT u IO Bool
startOfRound = withChecklist CL.notStarted

-- | Returns true if the lineup is empty or if all of the agents in the
--   lineup have had their turn at the CPU for this round.
endOfRound :: Universe u => StateT u IO Bool
endOfRound = withChecklist CL.done

-- | Creates a fresh lineup containing all of the agents in the
--   population, in random order.
refreshLineup :: Universe u => StateT u IO ()
refreshLineup = do
  as <- shuffledAgentIds
  withChecklist (CL.setItems as)

-- | Mark the current agent done. If it is still alive, it will move
--   to the end of the lineup.
markDone :: Universe u => A.AgentId -> StateT u IO ()
markDone = withChecklist . CL.markDone

shuffledAgentIds :: Universe u => StateT u IO [String]
shuffledAgentIds
  = agentIds >>= liftIO . evalRandIO . shuffle

data SimpleUniverse a = SimpleUniverse
  {
    suClock     :: K.PersistentCounter,
    suLogger    :: SL.SimpleLogger,
    suDB        :: FS.FSDatabase a,
    suNamer     :: N.SimpleNamer,
    suChecklist :: CL.PersistentChecklist
  } deriving (Read, Show, Eq)

instance (A.Agent a, D.Record a) => Universe (SimpleUniverse a) where
  type Agent (SimpleUniverse a) = a
  type Clock (SimpleUniverse a) = K.PersistentCounter
  clock = suClock
  setClock u c = u { suClock=c }
  type Logger (SimpleUniverse a) = SL.SimpleLogger
  logger = suLogger
  setLogger u l = u { suLogger=l }
  type AgentDB (SimpleUniverse a) = FS.FSDatabase a
  agentDB = suDB
  setAgentDB u d = u { suDB=d }
  type Namer (SimpleUniverse a) = N.SimpleNamer
  agentNamer = suNamer
  setNamer u n = u { suNamer=n }
  type Checklist (SimpleUniverse a) = CL.PersistentChecklist
  checklist = suChecklist
  setChecklist u cl = u { suChecklist=cl }

mkSimpleUniverse :: String -> FilePath -> SimpleUniverse a
mkSimpleUniverse name dir
  = SimpleUniverse c l d n cl
  where c = K.mkPersistentCounter (dir ++ "/clock")
        l = SL.mkSimpleLogger (dir ++ "/log/" ++ name ++ ".log")
        d = FS.mkFSDatabase (dir ++ "/db")
        n = N.mkSimpleNamer (name ++ "_") (dir ++ "/namer")
        cl = CL.mkPersistentChecklist (dir ++ "/todo")

data CachedUniverse a = CachedUniverse
  {
    cuClock     :: K.PersistentCounter,
    cuLogger    :: SL.SimpleLogger,
    cuDB        :: CFS.CachedFSDatabase a,
    cuNamer     :: N.SimpleNamer,
    cuChecklist :: CL.PersistentChecklist
  } deriving (Read, Show, Eq)

instance (A.Agent a, D.SizedRecord a) => Universe (CachedUniverse a) where
  type Agent (CachedUniverse a) = a
  type Clock (CachedUniverse a) = K.PersistentCounter
  clock = cuClock
  setClock u c = u { cuClock=c }
  type Logger (CachedUniverse a) = SL.SimpleLogger
  logger = cuLogger
  setLogger u l = u { cuLogger=l }
  type AgentDB (CachedUniverse a) = CFS.CachedFSDatabase a
  agentDB = cuDB
  setAgentDB u d = u { cuDB=d }
  type Namer (CachedUniverse a) = N.SimpleNamer
  agentNamer = cuNamer
  setNamer u n = u { cuNamer=n }
  type Checklist (CachedUniverse a) = CL.PersistentChecklist
  checklist = cuChecklist
  setChecklist u cl = u { cuChecklist=cl }

mkCachedUniverse :: String -> FilePath -> Int -> CachedUniverse a
mkCachedUniverse name dir cacheSize
  = CachedUniverse c l d n cl
  where c = K.mkPersistentCounter (dir ++ "/clock")
        l = SL.mkSimpleLogger (dir ++ "/log/" ++ name ++ ".log")
        d = CFS.mkCachedFSDatabase (dir ++ "/db") cacheSize
        n = N.mkSimpleNamer (name ++ "_") (dir ++ "/namer")
        cl = CL.mkPersistentChecklist (dir ++ "/todo")
