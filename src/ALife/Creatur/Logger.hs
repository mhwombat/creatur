------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Logger
-- Copyright   :  (c) Amy de Buitléir 2011-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple rotating log, tailored to the needs of the Créatúr 
-- framework.
--
------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ALife.Creatur.Logger
  (
    Logger(..),
    timestamp
  ) where

import Control.Monad.State (StateT)

#if MIN_VERSION_base(4,8,0)
import Data.Time (formatTime, getZonedTime, defaultTimeLocale)
#else
import Data.Time (formatTime, getZonedTime)
import System.Locale (defaultTimeLocale)
#endif

class Logger l where
  -- | @'writeToLog' msg@ formats and writes a new log message.
  writeToLog :: String -> StateT l IO ()

timestamp :: IO String
timestamp =
  fmap (formatTime defaultTimeLocale "%y%m%d%H%M%S%z") getZonedTime
