------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Logger
-- Copyright   :  (c) Amy de Buitléir 2011-2014
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

module ALife.Creatur.Logger
  (
    Logger(..),
    timestamp
  ) where

import Control.Monad.State (StateT)
import Data.Time (formatTime, getZonedTime)
import System.Locale (defaultTimeLocale)

class Logger l where
  -- | @'writeToLog' msg@ formats and writes a new log message.
  writeToLog :: String -> StateT l IO ()

timestamp :: IO String
timestamp =
  fmap (formatTime defaultTimeLocale "%y%m%d%H%M%S%z") getZonedTime
