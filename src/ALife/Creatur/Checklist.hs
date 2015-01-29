------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Checklist
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple task list which persists between runs.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module ALife.Creatur.Checklist
  (
    Checklist(..),
    PersistentChecklist,
    mkPersistentChecklist
  ) where

import ALife.Creatur.Persistent (Persistent, mkPersistent, getPS, putPS)
import Control.Monad (when)
import Control.Monad.State (StateT)
import qualified Data.List as L

type Status = ([String], [String]) -- (toDo, done)

class Checklist t where
  status :: StateT t IO Status
  markDone :: String -> StateT t IO ()
  notStarted :: StateT t IO Bool
  done :: StateT t IO Bool
  setItems :: [String] -> StateT t IO ()
  delete :: String -> StateT t IO ()

type PersistentChecklist = Persistent Status

-- | Creates a counter that will store its value in the specified file.
mkPersistentChecklist :: FilePath -> PersistentChecklist
mkPersistentChecklist = mkPersistent ([],[])

instance Checklist PersistentChecklist where
  status = getPS
  markDone x = do
    (ys,zs) <- getPS
    when (x `elem` ys) $ do
      putPS (L.delete x ys, zs ++ [x])
  notStarted = fmap (null . snd) getPS
  done = fmap (null . fst) getPS
  setItems ts = putPS (ts,[])
  delete tOld = do
    (xs,ys) <- getPS
    putPS (L.delete tOld xs, L.delete tOld ys)
