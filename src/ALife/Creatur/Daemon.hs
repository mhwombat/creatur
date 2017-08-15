------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Daemon
-- Copyright   :  (c) Amy de Buitléir 2012-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides a UNIX daemon to run an experiment using the Créatúr
-- framework.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module ALife.Creatur.Daemon
  (
    Job(..),
    CreaturDaemon(..),
    simpleDaemon,
    launch,
    launchInteractive,
    requestShutdown
  ) where

import Control.Concurrent (MVar, newMVar, readMVar, swapMVar, 
  threadDelay)
import Control.Exception (SomeException, handle, catch)
import Control.Monad (when)
import Control.Monad.State (StateT, execStateT)
import Foreign.C.String (withCStringLen)
import System.IO (hPutStr, stderr)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Posix.Daemonize as D
import System.Posix.Signals (Handler(Catch), fullSignalSet, 
  installHandler, sigTERM)
import System.Posix.Syslog (Priority(Warning), Facility(Daemon), syslog)
import System.Posix.User (getLoginName, getRealUserID)

termReceived :: MVar Bool
termReceived = unsafePerformIO (newMVar False)

-- | The work to be performed by a daemon.
data Job s = Job
  {
    -- | Operations to perform on startup.
    onStartup :: s -> IO s,
    -- | Operations to perform on shutdown.
    onShutdown :: s -> IO (),
    -- | Operations to perform if an exception occurs.
    onException :: s -> SomeException -> IO s,
    -- | Operations to perform repeatedly while running.
    task :: StateT s IO (),
    -- | Number of microseconds to sleep between invocations of @'task'@.
    sleepTime :: Int
  }

data CreaturDaemon p s = CreaturDaemon
  {
    daemon :: D.CreateDaemon p,
    job :: Job s
  }

-- | Creates a simple daemon to run a job. The daemon will run under
--   the login name.
simpleDaemon :: Job s -> s -> D.CreateDaemon ()
simpleDaemon j s = D.simpleDaemon { D.program = daemonMain j s,
                                    D.user    = Just "" }

-- | @'launch' daemon state@ creates a daemon, which invokes @daemon@.
--   *Note:* If @'user'@ (in @'daemon'@) is @Just ""@, the daemon will
--   run under the login name. If @'user'@ is Nothing, the daemon will
--   run under the name of the executable file containing the daemon. 
launch :: CreaturDaemon p s -> IO ()
launch d = do
  uid <- getRealUserID
  if uid /= 0
    then putStrLn "Must run as root"
    else do
      u <- defaultToLoginName (D.user . daemon $ d)
      D.serviced $ (daemon d) { D.user = u }

launchInteractive :: Job s -> s -> IO ()
launchInteractive j s = do
  s' <- onStartup j s
  daemonMain j s' ()
  return ()

defaultToLoginName :: Maybe String -> IO (Maybe String)
defaultToLoginName (Just "") = fmap Just getLoginName
defaultToLoginName x = return x

daemonMain :: Job s -> s -> () -> IO ()
daemonMain t s _ = do
  s' <- handle (onException t s) (onStartup t s)
  _ <- installHandler sigTERM (Catch requestShutdown)
        (Just fullSignalSet)
  _ <- wrap (daemonMainLoop t s')
  return ()

daemonMainLoop :: Job s -> s -> IO ()
daemonMainLoop t s = do
  let st = sleepTime t
  stopRequested <- readMVar termReceived
  when (not stopRequested) $ do
    s' <- handle (onException t s) $ execStateT (task t) s
    when (st > 0) $ threadDelay st
    daemonMainLoop t s'
  onShutdown t s

wrap :: IO () -> IO ()
wrap t = catch t
  (\e -> do
     let err = show (e :: SomeException)
     withCStringLen ("Unhandled exception: " ++ err) $ syslog (Just Daemon) Warning
     hPutStr stderr ("Unhandled exception: " ++ err)
     return ())

requestShutdown :: IO ()
requestShutdown = do
  _ <- swapMVar termReceived True
  return ()


