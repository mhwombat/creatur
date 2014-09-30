------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.EnergyPool
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- An exhaustible resource which persists between runs.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ALife.Creatur.EnergyPool
  (
    EnergyPool(..),
    PersistentEnergyPool,
    mkPersistentEnergyPool
  ) where

import ALife.Creatur.Persistent (Persistent, mkPersistent, getPS, putPS)
import Control.Monad.State (StateT)

class EnergyPool e where
  replenish :: Double -> StateT e IO ()
  withdraw :: Double -> StateT e IO Double
  available :: StateT e IO Double

type PersistentEnergyPool = Persistent Double

-- | Creates a resource that will store its value in the specified file.
mkPersistentEnergyPool :: FilePath -> PersistentEnergyPool
mkPersistentEnergyPool = mkPersistent 0

instance EnergyPool PersistentEnergyPool where
  replenish = putPS
  withdraw xWanted = do
    xTotal <- getPS
    let x = min xWanted xTotal
    putPS (xTotal - x)
    return x
  available = getPS
