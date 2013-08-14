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
--   The program must return the agent (which may have been modified).
--   (The universe will then be updated with these changes.)
type AgentProgram c l d n x a = a -> StateT (Universe c l d n x a) IO a

withAgent :: (Clock c, Logger l, Database d, Agent a, Serialize a, 
    Record a, a ~ DBRecord d) =>
  AgentProgram c l d n x a -> AgentId -> 
    StateT (Universe c l d n x a) IO ()
withAgent program name = 
  (zoom agentDB . D.lookup) name >>= withAgent' program name

withAgent' :: (Clock c, Logger l, Database d, Agent a, Serialize a, 
    Record a, a ~ DBRecord d) =>
  AgentProgram c l d n x a -> AgentId -> Either String a -> 
    StateT (Universe c l d n x a) IO ()
withAgent' _ name (Left msg) = 
  writeToLog $ "Unable to read '" ++ name ++ "': " ++ msg
withAgent' program _ (Right a) = 
  program a >>= zoom agentDB . storeOrArchive

runNoninteractingAgents :: (Clock c, Logger l, Database d, Agent a, Serialize a, 
    Record a, a ~ DBRecord d) =>
  AgentProgram c l d n x a -> StateT (Universe c l d n x a) IO ()
runNoninteractingAgents agentProgram = do
  xs <- agentIds
  xs' <- liftIO $ evalRandIO $ shuffle xs
  -- TODO Write out current list so we can pick up where we left off????
  --      (when (currentTime logger `mod` 1000 == 0) $ logStats universe logger)
  mapM_ (withAgent agentProgram) xs'
  zoom clock incTime

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
--   The program must return a list of agents that it has modified.
--   (The universe will then be updated with these changes.)
--   The order of the output list is not important.
type AgentsProgram c l d n x a = 
  [a] -> StateT (Universe c l d n x a) IO [a]

withAgents :: (Clock c, Logger l, Database d, Agent a, Serialize a, 
    Record a, a ~ DBRecord d) =>
  AgentsProgram c l d n x a -> [AgentId] ->
    StateT (Universe c l d n x a) IO ()
withAgents program names = 
  (zoom agentDB . multiLookup) names >>= withAgents' program

withAgents' :: (Clock c, Logger l, Database d, Agent a, Serialize a, 
  Record a, a ~ DBRecord d) =>
    AgentsProgram c l d n x a -> Either String [a] ->
      StateT (Universe c l d n x a) IO ()
withAgents' _ (Left msg) = 
  writeToLog $ "Database error: " ++ msg
withAgents' program (Right as) = 
  program as >>= mapM_ (zoom agentDB . storeOrArchive)

runInteractingAgents :: (Clock c, Logger l, Database d, Agent a, 
  Serialize a, Record a, a ~ DBRecord d) =>
    AgentsProgram c l d n x a -> StateT (Universe c l d n x a) IO ()
runInteractingAgents agentsProgram = do
  xs <- agentIds
  xs' <- liftIO $ evalRandIO $ shuffle xs
  mapM_ (withAgents agentsProgram) $ makeViews xs'
  zoom clock incTime

makeViews :: [a] -> [[a]]
makeViews as = unfoldr f (0,as)
  where f (n,xs) = if n == length xs then Nothing else Just (rotate xs,(n+1,rotate xs))

