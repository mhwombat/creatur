------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Daemon
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides a UNIX daemon to run an experiment using the Créatúr
-- framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module ALife.Creatur.Daemon
  (
    Daemon(..),
    launch
  ) where

import Control.Concurrent (MVar, newMVar, readMVar, swapMVar, 
  threadDelay)
import Control.Exception (SomeException, handle)
import Control.Monad.State (StateT, execStateT)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Daemonize (CreateDaemon(..), serviced, simpleDaemon)
import System.Posix.Signals (Handler(Catch), fullSignalSet, 
  installHandler, sigTERM)
import System.Posix.User (getLoginName, getRealUserID)

termReceived :: MVar Bool
termReceived = unsafePerformIO (newMVar False)

-- | Daemon configuration.
--   If @username@ == "", the daemon will run under the login name.
data Daemon s = Daemon
  {
    onStartup :: s -> IO s,
    onShutdown :: s -> IO (),
    onException :: s -> SomeException -> IO s,
    task :: StateT s IO (),
    username :: String,
    sleepTime :: Int
  }

-- | @'launch' username sleepTime state task@ creates a daemon
--   running as @username@, which invokes @task@ repeatedly, sleeping 
--   for @sleepTime@ microseconds between invocations of @task@.
launch :: Daemon s -> s -> IO ()
launch d s = do
  uid <- getRealUserID
  if uid /= 0
    then putStrLn "Must run as root"
    else do
      u <- daemonUsername d
      serviced $ simpleDaemon 
        { program = daemonMain d s,
          user    = Just u }

daemonUsername :: Daemon s -> IO String
daemonUsername d =
  if (null . username) d
    then getLoginName
    else (return . username) d
    
daemonMain :: Daemon s -> s -> () -> IO ()
daemonMain d s _ = do
  s' <- onStartup d s
  _ <- installHandler sigTERM (Catch handleTERM) (Just fullSignalSet)
  _ <- daemonMainLoop d s'
  return ()

daemonMainLoop :: Daemon s -> s -> IO ()
daemonMainLoop d s = do
  threadDelay . sleepTime $ d
  timeToStop <- readMVar termReceived
  if timeToStop 
    then onShutdown d s
    else do
      s' <- handle (onException d s) $ execStateT (task d) s
      daemonMainLoop d s'

handleTERM :: IO ()
handleTERM = do
  _ <- swapMVar termReceived True
  return ()

