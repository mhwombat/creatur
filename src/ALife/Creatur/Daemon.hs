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
    TaskConfig(..),
    Daemon(..),
    launch,
    requestShutdown
  ) where

import Control.Concurrent (MVar, newMVar, readMVar, swapMVar, 
  threadDelay)
import Control.Exception (SomeException, handle, catch)
import Control.Monad (when)
import Control.Monad.State (StateT, execStateT)
import System.IO (hPutStr, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Daemonize (CreateDaemon(..), serviced, simpleDaemon)
import System.Posix.Signals (Handler(Catch), fullSignalSet, 
  installHandler, sigTERM)
import System.Posix.User (getLoginName, getRealUserID)

termReceived :: MVar Bool
termReceived = unsafePerformIO (newMVar False)

-- | Task configuration.
data TaskConfig s = TaskConfig
  {
    -- | Operations to perform on startup.
    onStartup :: s -> IO s,
    -- | Operations to perform on shutdown.
    onShutdown :: s -> IO (),
    -- | Operations to perform if an exception occurs.
    onException :: s -> SomeException -> IO s,
    -- | Operations to perform repeatedly while running.
    run :: StateT s IO (),
    -- | Number of microseconds to sleep between invocations of @'run'@.
    sleepTime :: Int
  }

data Daemon p s = Daemon
  {
    daemon :: CreateDaemon p,
    task :: TaskConfig s
  }

-- | @'launch' daemon state@ creates a daemon, which invokes @task@.
--   *Note:* If @'user'@ (in @'daemon'@) is @Just ""@, the daemon will
--   run under the login name. If @'user'@ is Nothing, the daemon will
--   run under the name of the executable file containing the daemon. 
launch :: Daemon p s -> s -> IO ()
launch d s = do
  uid <- getRealUserID
  if uid /= 0
    then putStrLn "Must run as root"
    else do
      u <- defaultToLoginName (user . daemon $ d)
      serviced $ simpleDaemon 
        { program = daemonMain d s,
          user    = u }

defaultToLoginName :: Maybe String -> IO (Maybe String)
defaultToLoginName (Just "") = fmap Just getLoginName
defaultToLoginName x = return x

daemonMain :: Daemon p s -> s -> () -> IO ()
daemonMain d s _ = do
  s' <- handle ((onException . task $ d) s) ((onStartup . task $ d) s)
  _ <- installHandler sigTERM (Catch requestShutdown)
        (Just fullSignalSet)
  _ <- wrap (daemonMainLoop d s')
  return ()

daemonMainLoop :: Daemon p s -> s -> IO ()
daemonMainLoop d s = do
  let st = sleepTime . task $ d
  stopRequested <- readMVar termReceived
  when (not stopRequested) $ do
    s' <- handle ((onException . task $ d) s) $
           execStateT (run . task $ d) s
    when (st > 0) $ threadDelay st
    daemonMainLoop d s'
  (onShutdown . task $ d) s

wrap :: IO () -> IO ()
wrap t = catch t
  (\e -> do
     let err = show (e :: SomeException)
     hPutStr stderr ("Unhandled exception: " ++ err)
     return ())

requestShutdown :: IO ()
requestShutdown = do
  _ <- swapMVar termReceived True
  return ()


