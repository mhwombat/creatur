------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Tools.Logger
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple counter which persists between runs.
--
------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax #-}
module ALife.Creatur.Counter
  (
    Counter(..),
    PersistentCounter,
    mkPersistentCounter
  ) where

import ALife.Creatur.Clock (Clock, currentTime, incTime)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, gets, modify, put)
import System.Directory (doesFileExist)

class Counter c where
  current ∷ StateT c IO Int
  increment ∷ StateT c IO ()

data PersistentCounter = PersistentCounter {
    initialised ∷ Bool,
    time ∷ Int,
    filename ∷ FilePath
  } deriving Show

-- | Creates a counter that will store its value in the specified file.
mkPersistentCounter ∷ FilePath → PersistentCounter
mkPersistentCounter = PersistentCounter False (-1)

instance Counter PersistentCounter where
  current = do
    initIfNeeded
    gets time
  increment = do
    t ← current
    let t' = t + 1
    f ← gets filename
    modify (\c → c { time=t' })
    liftIO $ writeFile f $ show t'

initIfNeeded ∷ StateT PersistentCounter IO ()
initIfNeeded = do
  isInitialised ← gets initialised
  unless isInitialised $ do
    counter ← get
    counter' ← liftIO $ initialise counter
    put counter'

initialise ∷ PersistentCounter → IO PersistentCounter
initialise counter = do
  let f = filename counter
  fExists ← doesFileExist f
  if fExists
    then do
      s ← readFile f
      return $ counter { initialised=True, time=read s }
    else return $ counter { initialised=True, time=0 }

instance Clock PersistentCounter where
  currentTime = current
  incTime = increment

