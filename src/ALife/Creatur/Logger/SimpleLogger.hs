------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Logger.SimpleLogger
-- Copyright   :  (c) Amy de Buitléir 2011-2017
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

module ALife.Creatur.Logger.SimpleLogger
  (
    SimpleLogger,
    mkSimpleLogger
  ) where

import ALife.Creatur.Util (getLift)
import ALife.Creatur.Logger (Logger(..), timestamp)
import Control.Conditional (unlessM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, gets, modify)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitFileName)

-- | A rotating logger.
data SimpleLogger = SimpleLogger {
    initialised :: Bool,
    logFilename :: FilePath
  } deriving (Show, Eq)

-- | @'mkSimpleLogger' f@ creates a logger that will write to
--   file @f@.
mkSimpleLogger :: FilePath -> SimpleLogger
mkSimpleLogger f = SimpleLogger False f

instance Logger SimpleLogger where
  writeToLog msg = do
    initIfNeeded
    getLift $ write' msg

initIfNeeded :: StateT SimpleLogger IO ()
initIfNeeded =
  unlessM (gets initialised) initialise

initialise :: StateT SimpleLogger IO ()
initialise = do
  (d,_) <- fmap splitFileName $ gets logFilename
  liftIO $ createDirectoryIfMissing True d
  modify (\l -> l { initialised=True } )

write' :: String -> SimpleLogger -> IO ()
write' msg logger = do
  ts <- timestamp
  appendFile (logFilename logger) $ ts ++ "\t" ++ msg ++ "\n"
