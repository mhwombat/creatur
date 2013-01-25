------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Tools.Logger
-- Copyright   :  (c) Amy de Buitléir 2011-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple rotating log, tailored to the needs of the Créatúr 
-- framework.
--
------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ALife.Creatur.Logger
  (
    Logger(..),
    SimpleRotatingLogger,
    mkSimpleRotatingLogger
  ) where

import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, gets, put)
import Data.Eq.Unicode ((≡))
import Data.Time (formatTime, getZonedTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.Locale (defaultTimeLocale)

class Logger l where
  -- | @'write' msg@ formats and writes a new log message.
  writeToLog ∷ String → StateT l IO ()

-- | A rotating logger.
data SimpleRotatingLogger = SimpleRotatingLogger {
    initialised ∷ Bool,
    directory ∷ FilePath,
    logFilename ∷ FilePath,
    expFilename ∷ FilePath,
    maxRecordsPerFile ∷ Int,
    recordCount ∷ Int
  } deriving Show

-- | @'mkSimpleRotatingLogger' d prefix n@ creates a logger that will write to
--   directory @d@. The log \"rotates\" (starts a new log file) every @n@
--   records. Log files follow the naming convention @prefix@./k/, where /k/ 
--   is the number of the last log record contained in the file. If logging
--   has already been set up in @directory@, then logging will continue where
--   it left off; appending to the most recent log file.
mkSimpleRotatingLogger ∷ FilePath → String → Int → SimpleRotatingLogger
mkSimpleRotatingLogger d pre n = SimpleRotatingLogger False d fLog fExp n (-1)
  where fLog = d ++ "/" ++ pre ++ ".log"
        fExp = d ++ "/" ++ pre ++ ".exp"

instance Logger SimpleRotatingLogger where
  writeToLog msg = do
    initIfNeeded
    logger ← get
    logger' ← liftIO $ bumpRecordCount logger
    put logger'
    liftIO $ write' logger' msg

initIfNeeded ∷ StateT SimpleRotatingLogger IO ()
initIfNeeded = do
  isInitialised ← gets initialised
  unless isInitialised $ do
    logger ← get
    logger' ← liftIO $ initialise logger
    put logger'

initialise ∷ SimpleRotatingLogger → IO SimpleRotatingLogger
initialise logger = do
  createDirectoryIfMissing True (directory logger)
  let fExp = expFilename logger
  expFileExists ← doesFileExist fExp
  if expFileExists
    then do
      s ← readFile fExp
      return $ logger { initialised=True, recordCount=read s}
    else return $ logger { initialised=True, recordCount=0}

write' ∷ SimpleRotatingLogger → String → IO ()
write' logger msg = do
  timestamp ←
      fmap (formatTime defaultTimeLocale "%y%m%d%H%M%S%z") getZonedTime
  appendFile (logFilename logger)
    $ timestamp ++ "\t" ++ msg ++ "\n"

bumpRecordCount ∷ SimpleRotatingLogger → IO SimpleRotatingLogger
bumpRecordCount logger = do
  let n = 1 + recordCount logger
  when (0 ≡ n `mod` maxRecordsPerFile logger) $ liftIO $ rotateLog logger
  writeFile (expFilename logger) (show n)
  return logger{ recordCount=n }

rotateLog ∷ SimpleRotatingLogger → IO ()
rotateLog logger = do
  let f = logFilename logger
  renameFile f $ f ++ '.' : (show . recordCount) logger

