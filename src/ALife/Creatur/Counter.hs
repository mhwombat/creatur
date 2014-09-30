------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Counter
-- Copyright   :  (c) Amy de Buitléir 2012-2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple counter which persists between runs.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ALife.Creatur.Counter
  (
    Counter(..),
    PersistentCounter,
    mkPersistentCounter
  ) where

import ALife.Creatur.Clock (Clock, currentTime, incTime)
import ALife.Creatur.Persistent (Persistent, mkPersistent, getPS, putPS)
import Control.Monad.State (StateT)

class Counter c where
  current :: StateT c IO Int
  increment :: StateT c IO ()

type PersistentCounter = Persistent Int

-- | Creates a counter that will store its value in the specified file.
mkPersistentCounter :: FilePath -> PersistentCounter
mkPersistentCounter = mkPersistent 0

instance Counter PersistentCounter where
  current = getPS
  increment = do
    k <- getPS
    putPS (k+1)

instance Clock PersistentCounter where
  currentTime = current
  incTime = increment
