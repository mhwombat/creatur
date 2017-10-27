------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Clock
-- Copyright   :  (c) Amy de Buitléir 2012-2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- An internal simulation clock which persists between runs. This is a
-- simple counter, completely independent from any system clock or
-- hardware clock. The clock does not automatically advance, it only
-- advances when @'incTime'@ is called. In this way, the Créatúr 
-- framework will run consistently, treating all agents fairly,
-- regardless of current processor load. It also ensures that data
-- obtained from simulation runs on different machines with different
-- CPU performance can still be meaningfully compared.
--
------------------------------------------------------------------------
module ALife.Creatur.Clock
  (
    Clock(..)
  ) where

import ALife.Creatur (Time)
import Control.Monad.State (StateT)

-- | A clock representing the time in a Créatúr universe.
--   It is used to schedule events and ensure that each agent gets its
--   fair share of the CPU.
--   This clock is entirely separate from the system clock.
--   It advances only when @'incTime'@ is called.
--   This allows Créatúr to run without being affected by other
--   processes which might be using the CPU at the same time.
class Clock c where
  -- | The current time, measured in "ticks"
  currentTime :: StateT c IO Time
  -- | Advance the clock to the next "tick".
  incTime :: StateT c IO ()
