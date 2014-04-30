------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Daemon
-- Copyright   :  (c) Amy de Buitléir 2012-2014
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
import Control.Exception (SomeException, handle, catch)
import Control.Monad (when)
import Control.Monad.State (StateT, runStateT)
import System.IO (hPutStr, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Daemonize (CreateDaemon(..), serviced, simpleDaemon)
import System.Posix.Signals (Handler(Catch), fullSignalSet, 
  installHandler, sigTERM)
import System.Posix.User (getLoginName, getRealUserID)

termReceived :: MVar Bool
termReceived = unsafePerformIO (newMVar False)

-- | Daemon configuration.
--   If @username@ is null, the daemon will run under the login name.
data Daemon s = Daemon
  {
    onStartup :: s -> IO s,
    onShutdown :: s -> IO (),
    onException :: s -> SomeException -> IO (Bool, s),
    -- | The agent task.
    task :: StateT s IO Bool,
    username :: String,
    -- | Number of microseconds to sleep between agent tasks.
    sleepTime :: Int
  }

-- | @'launch' daemon state@ creates a daemon running under the current
--   user's real userID, which invokes @task@.
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
  _ <- wrap (daemonMainLoop d s')
  return ()

daemonMainLoop :: Daemon s -> s -> IO ()
daemonMainLoop d s = do
  stopRequested <- readMVar termReceived
  when (not stopRequested) $ do
    (continue, s') <- handle (onException d s) $ runStateT (task d) s
    when continue $ do
      threadDelay $ sleepTime d
      daemonMainLoop d s'
  onShutdown d s

wrap :: IO () -> IO ()
wrap t = catch t
  (\e -> do
     let err = show (e :: SomeException)
     hPutStr stderr ("Unhandled exception: " ++ err)
     return ())

handleTERM :: IO ()
handleTERM = do
  _ <- swapMVar termReceived True
  return ()

