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
    noSummary
 ) where

import ALife.Creatur.Daemon (Daemon(..))
import ALife.Creatur.Universe (Universe, Agent, AgentProgram,
  AgentsProgram, writeToLog, lineup, refresh, markDone, endOfRound,
  withAgent, withAgents, incTime)
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
    => AgentProgram u -> SummaryProgram u -> StateT u IO Bool
runNoninteractingAgents agentProgram summaryProgram = do
  atEndOfRound summaryProgram
  as <- lineup
  if null as
    then return False
    else do
      let a = head as
      markDone a
      -- do that first in case the next line triggers an exception
      withAgent agentProgram a
      return True

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
    => AgentsProgram u -> (Int, Int) -> SummaryProgram u -> StateT u IO Bool
runInteractingAgents agentsProgram (minAgents, maxAgents) summaryProgram = do
  atEndOfRound summaryProgram
  as <- lineup
  let n = length as
  writeToLog $ "Pop. size=" ++ show n
  if n < minAgents
    then do
      writeToLog "Population too small"
      summaryProgram
      writeToLog "Requesting shutdown (population too small)"
      return False
    else
      if n > maxAgents
        then do
          writeToLog "Population too big"
          summaryProgram
          writeToLog "Requesting shutdown (population too big)"
          return False
        else do
          markDone (head as)
          -- do that first in case the next line triggers an exception
          withAgents agentsProgram as
          return True

atEndOfRound
  :: Universe u 
    => SummaryProgram u -> StateT u IO ()
atEndOfRound summaryProgram = do
  whenM endOfRound $ do
    writeToLog "End of round"
    summaryProgram
    refresh
    incTime
    writeToLog "Beginning of round"

-- | A program that processes the outputs from all the agent programs.
--   For example, this program might aggregate the statistics and
--   record the result.
type SummaryProgram u = StateT u IO ()

noSummary :: SummaryProgram u
noSummary = return ()

-- Note: There's no reason for the checklist type to be a parameter of
-- the Universe type. Users don't interact directly with it, so they
-- won't have any reason to want to use a different checklist
-- implementation.
