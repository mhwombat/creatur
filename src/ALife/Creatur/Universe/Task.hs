------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Universe.Task
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides tasks that you can use with a daemon. These tasks handle
-- reading and writing agents, which reduces the amount of code you
-- need to write. 
--
-- It’s also easy to write your own tasks, using these as a guide.)
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module ALife.Creatur.Universe.Task
 (
    AgentProgram,
    AgentsProgram,
    SummaryProgram,
    noSummary,
    withAgent,
    withAgents,
    runNoninteractingAgents,
    runInteractingAgents,
    simpleDaemon,
    startupHandler,
    shutdownHandler,
    exceptionHandler
 ) where

import ALife.Creatur (Agent, AgentId)
import ALife.Creatur.Clock (Clock, incTime)
import ALife.Creatur.Daemon (Daemon(..))
import ALife.Creatur.Database as D (Database, DBRecord, Record,
  lookup)
import ALife.Creatur.Logger (Logger, writeToLog)
import ALife.Creatur.Util (rotate, shuffle)
import ALife.Creatur.Universe (Universe, agentDB, clock, multiLookup,
  agentIds, storeOrArchive)
import Control.Exception (SomeException)
import Control.Lens (zoom)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.State (StateT, execStateT)
import Data.List (unfoldr)
import Data.Maybe (catMaybes)
import Data.Serialize (Serialize)

simpleDaemon :: Logger u => Daemon u
simpleDaemon = Daemon
  {
    onStartup = startupHandler,
    onShutdown = shutdownHandler,
    onException = exceptionHandler,
    task = undefined,
    username = "",
    sleepTime = 100000
  }

startupHandler :: Logger u => u -> IO u
startupHandler = execStateT (writeToLog "Starting")

shutdownHandler :: Logger u => u -> IO ()
shutdownHandler u = do
  _ <- execStateT (writeToLog "Shutdown requested") u
  return ()

exceptionHandler :: Logger u => u -> SomeException -> IO u
exceptionHandler u x = execStateT (writeToLog ("WARNING: " ++ show x)) u

-- | A program for an agent which doesn't interact with other agents.
--   The input parameter is the agent whose turn it is to use the CPU.
--   The program must return the agent (which may have been modified),
--   along with any data (e.g., statistics) to be used by the summary
--   program.
type AgentProgram c l d n x a s
  = a -> StateT (Universe c l d n x a) IO (a, Maybe s)

-- | A program that processes the outputs from all the agent programs.
--   For example, this program might aggregate the statistics and
--   record the result.
type SummaryProgram c l d n x a s
  = [s] -> StateT (Universe c l d n x a) IO ()

noSummary :: SummaryProgram c l d n x a s
noSummary _ = return ()

withAgent :: (Clock c, Logger l, Database d, Agent a, Serialize a, 
    Record a, a ~ DBRecord d) =>
  AgentProgram c l d n x a s -> AgentId -> 
    StateT (Universe c l d n x a) IO (Maybe s)
withAgent program name = 
  (zoom agentDB . D.lookup) name >>= withAgent' program name

withAgent' :: (Clock c, Logger l, Database d, Agent a, Serialize a, 
    Record a, a ~ DBRecord d) =>
  AgentProgram c l d n x a s -> AgentId -> Either String a -> 
    StateT (Universe c l d n x a) IO (Maybe s)
withAgent' _ name (Left msg) = do
  writeToLog $ "Unable to read '" ++ name ++ "': " ++ msg
  return Nothing
withAgent' program _ (Right a) = do
  (a', s) <- program a
  zoom agentDB $ storeOrArchive a'
  return s

runNoninteractingAgents
  :: (Clock c, Logger l, Database d, Agent a, Serialize a, Record a,
    a ~ DBRecord d)
      => AgentProgram c l d n x a s -> SummaryProgram c l d n x a s
        -> StateT (Universe c l d n x a) IO ()
runNoninteractingAgents agentProgram summaryProgram = do
  writeToLog "Beginning of round"
  xs <- agentIds
  xs' <- liftIO $ evalRandIO $ shuffle xs
  writeToLog $ "Lineup: " ++ show xs'
  ys <- mapM (withAgent agentProgram) xs'
  summaryProgram $ catMaybes ys
  zoom clock incTime
  writeToLog "End of round"

-- | A program which allows an agent to interact with one or more of
--   its neighbours.
--
--   The input parameter is a list of agents. The first agent in the
--   list is the agent whose turn it is to use the CPU. The rest of
--   the list contains agents it could interact with. For example, if
--   agents reproduce sexually, the program might check if the first
--   agent in the list is female, and the second one is male, and if so,
--   mate them to produce offspring. The input list is generated in a
--   way that guarantees that every possible sequence of agents has an
--   equal chance of occurring.
--
--   The program must return a list of agents that it has modified,
--   along with any data (e.g., statistics) to be used by the summary
--   program.
--   The order of the output list is not important.
type AgentsProgram c l d n x a s = 
  [a] -> StateT (Universe c l d n x a) IO ([a], Maybe s)

withAgents
  :: (Clock c, Logger l, Database d, Agent a, Serialize a, Record a,
    a ~ DBRecord d)
     => AgentsProgram c l d n x a s -> [AgentId]
       -> StateT (Universe c l d n x a) IO (Maybe s)
withAgents program names = 
  (zoom agentDB . multiLookup) names >>= withAgents' program

withAgents'
  :: (Clock c, Logger l, Database d, Agent a, Serialize a, 
    Record a, a ~ DBRecord d)
     => AgentsProgram c l d n x a s -> Either String [a]
       -> StateT (Universe c l d n x a) IO (Maybe s)
withAgents' _ (Left msg) = do
  writeToLog $ "Database error: " ++ msg
  return Nothing
withAgents' program (Right as) = do
  (as', s) <- program as
  mapM_ (zoom agentDB . storeOrArchive) as'
  return s

runInteractingAgents
  :: (Clock c, Logger l, Database d, Agent a, Serialize a, Record a,
    a ~ DBRecord d)
     => AgentsProgram c l d n x a s -> SummaryProgram c l d n x a s
       -> StateT (Universe c l d n x a) IO ()
runInteractingAgents agentsProgram summaryProgram = do
  writeToLog "Beginning of round"
  xs <- agentIds
  xs' <- liftIO $ evalRandIO $ shuffle xs
  writeToLog $ "Lineup: " ++ show xs'
  ys <- mapM (withAgents agentsProgram) $ makeViews xs'
  summaryProgram $ catMaybes ys
  zoom clock incTime
  writeToLog "End of round"

makeViews :: [a] -> [[a]]
makeViews as = unfoldr f (0,as)
  where f (n,xs) = if n == length xs then Nothing else Just (rotate xs,(n+1,rotate xs))

