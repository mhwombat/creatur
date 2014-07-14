------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Task
-- Copyright   :  (c) Amy de Buitléir 2012-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides tasks that you can use with a daemon. These tasks handle
-- reading and writing agents, and various other housekeeping chores,
-- which reduces the amount of code you need to write. 
--
-- It’s also easy to write your own tasks, using these as a guide.)
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module ALife.Creatur.Task
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
    exceptionHandler,
    nothing
 ) where

import ALife.Creatur.Daemon (Daemon(..))
import ALife.Creatur.Universe (Universe, Agent, AgentProgram,
  AgentsProgram, writeToLog, lineup, refreshLineup, markDone, endOfRound,
  withAgent, withAgents, incTime, popSize)
import Control.Conditional (whenM)
import Control.Exception (SomeException)
import Control.Monad.State (StateT, execStateT)
import Data.Serialize (Serialize)

simpleDaemon :: Universe u => Daemon u
simpleDaemon = Daemon
  {
    onStartup = startupHandler,
    onShutdown = shutdownHandler,
    onException = exceptionHandler,
    task = undefined,
    username = "",
    sleepTime = 100
  }

startupHandler :: Universe u => u -> IO u
startupHandler = execStateT (writeToLog $ "Starting")

shutdownHandler :: Universe u => u -> IO ()
shutdownHandler u = do
  _ <- execStateT (writeToLog "Shutdown requested") u
  return ()

exceptionHandler :: Universe u => u -> SomeException -> IO (Bool, u)
exceptionHandler u x = do
  u' <- execStateT (writeToLog ("WARNING: " ++ show x)) u
  return (True, u')

runNoninteractingAgents
  :: (Universe u, Serialize (Agent u))
    => AgentProgram u -> (Int, Int) -> StateT u IO () -> StateT u IO ()
      -> StateT u IO Bool
runNoninteractingAgents agentProgram popRange startRoundProgram
    endRoundProgram = do
  atStartOfRound startRoundProgram
  as <- lineup
  if null as
    then return False
    else do
      let a = head as
      markDone a
      -- do that first in case the next line triggers an exception
      withAgent agentProgram a
      atEndOfRound endRoundProgram
      popSizeInBounds popRange

--   The input parameter is a list of agents. The first agent in the
--   list is the agent whose turn it is to use the CPU. The rest of
--   the list contains agents it could interact with. For example, if
--   agents reproduce sexually, the program might check if the first
--   agent in the list is female, and the second one is male, and if so,
--   mate them to produce offspring. The input list is generated in a
--   way that guarantees that every possible sequence of agents has an
--   equal chance of occurring.

runInteractingAgents
  :: (Universe u, Serialize (Agent u))
    => AgentsProgram u -> (Int, Int) -> StateT u IO () -> StateT u IO ()
      -> StateT u IO Bool
runInteractingAgents agentsProgram popRange startRoundProgram
    endRoundProgram = do
  atStartOfRound startRoundProgram
  as <- lineup
  markDone (head as)
  -- do that first in case the next line triggers an exception
  withAgents agentsProgram as
  atEndOfRound endRoundProgram
  popSizeInBounds popRange

popSizeInBounds :: Universe u => (Int, Int) -> StateT u IO Bool
popSizeInBounds (minAgents, maxAgents) = do
  n <- popSize
  if n < minAgents
    then do
      writeToLog "Requesting shutdown (population too small)"
      return False
    else
      if n > maxAgents
        then do
          writeToLog "Requesting shutdown (population too big)"
          return False
        else return True

atStartOfRound :: Universe u => StateT u IO () -> StateT u IO ()
atStartOfRound program = do
  whenM endOfRound $ do
    refreshLineup
    incTime
    writeToLog "Beginning of round"
    program

atEndOfRound :: Universe u => StateT u IO () -> StateT u IO ()
atEndOfRound program = do
  whenM endOfRound $ do
    writeToLog "End of round"
    program

nothing :: StateT u IO ()
nothing = return ()

-- Note: There's no reason for the checklist type to be a parameter of
-- the Universe type. Users don't interact directly with it, so they
-- won't have any reason to want to use a different checklist
-- implementation.
