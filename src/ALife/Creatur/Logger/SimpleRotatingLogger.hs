------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Logger.SimpleRotatingLogger
-- Copyright   :  (c) 2011-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple rotating log, tailored to the needs of the Créatúr
-- framework.
--
------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ALife.Creatur.Logger.SimpleRotatingLogger
  (
    Logger(..),
    SimpleRotatingLogger,
    mkSimpleRotatingLogger
  ) where

import           ALife.Creatur.Logger   (Logger (..), timestamp)
import           ALife.Creatur.Util     (getLift)
import           Control.Conditional    (unlessM, whenM)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (StateT, gets, modify)
import           System.Directory       (createDirectoryIfMissing,
                                         doesFileExist, renameFile)

-- | A rotating logger.
data SimpleRotatingLogger = SimpleRotatingLogger {
    initialised       :: Bool,
    directory         :: FilePath,
    logFilename       :: FilePath,
    expFilename       :: FilePath,
    maxRecordsPerFile :: Int,
    recordCount       :: Int,
    logCount          :: Int
  } deriving (Read, Show, Eq)

-- | @'mkSimpleRotatingLogger' d prefix n@ creates a logger that will write to
--   directory @d@. The log \"rotates\" (starts a new log file) every @n@
--   records. Log files follow the naming convention @prefix@./k/, where /k/
--   is the number of the last log record contained in the file. If logging
--   has already been set up in @directory@, then logging will continue where
--   it left off; appending to the most recent log file.
mkSimpleRotatingLogger :: FilePath -> String -> Int -> SimpleRotatingLogger
mkSimpleRotatingLogger d pre n =
  SimpleRotatingLogger False d fLog fExp n 0 0
  where fLog = d ++ "/" ++ pre ++ ".log"
        fExp = d ++ "/" ++ pre ++ ".exp"

instance Logger SimpleRotatingLogger where
  writeToLog msg = do
    initIfNeeded
    bumpRecordCount
    rotateLogIfNeeded
    getLift $ write' msg
    saveState

initIfNeeded :: StateT SimpleRotatingLogger IO ()
initIfNeeded =
  unlessM (gets initialised) initialise

initialise :: StateT SimpleRotatingLogger IO ()
initialise = do
  d <- gets directory
  liftIO $ createDirectoryIfMissing True d
  fExp <- gets expFilename
  whenM (liftIO $ doesFileExist fExp) readState
  modify (\l -> l { initialised=True } )
  debug "initialise"

debug :: String -> StateT SimpleRotatingLogger IO ()
debug s = do
  n <- gets recordCount
  k <- gets logCount
  getLift . write' $ "DEBUG " ++ s ++ ": n=" ++ show n ++ ": k=" ++ show k
  fExp <- gets expFilename
  whenM (liftIO $ doesFileExist fExp) $ do
    s' <- liftIO $ readFile fExp
    let (n',k') = read s' :: (Int,Int)
    getLift . write' $ "DEBUG2 " ++ s ++ ": n'=" ++ show n' ++ ": k'=" ++ show k'

readState :: StateT SimpleRotatingLogger IO ()
readState = do
  fExp <- gets expFilename
  s <- liftIO $ readFile fExp
  let (n,k) = read s
  modify (\l -> l { recordCount=n, logCount=k } )

saveState :: StateT SimpleRotatingLogger IO ()
saveState = do
  e <- gets expFilename
  n <- gets recordCount
  k <- gets logCount
  liftIO . writeFile e $ "(" ++ show n ++ "," ++ show k ++ ")"

write' :: String -> SimpleRotatingLogger -> IO ()
write' msg logger = do
  ts <- timestamp
  appendFile (logFilename logger) $ ts ++ "\t" ++ msg ++ "\n"

bumpRecordCount :: StateT SimpleRotatingLogger IO ()
bumpRecordCount = modify (\l -> l { recordCount=recordCount l + 1 })

rotateLogIfNeeded :: StateT SimpleRotatingLogger IO ()
rotateLogIfNeeded = do
  debug "rotateLogIfNeeded"
  n <- gets recordCount
  m <- gets maxRecordsPerFile
  when (n >= m) rotateLog

rotateLog :: StateT SimpleRotatingLogger IO ()
rotateLog = do
  debug "rotateLog"
  f <- gets logFilename
  n <- gets logCount
  let fPrev = f ++ '.' : show n
  getLift . write' $ "Continued in log " ++ show (n+1)
  liftIO $ renameFile f fPrev
  getLift . write' $ "Continued from log " ++ show n
  modify (\l -> l { recordCount=0, logCount=n+1 })
