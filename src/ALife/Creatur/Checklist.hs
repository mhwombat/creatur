------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Checklist
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple task list which persists between runs.
--
------------------------------------------------------------------------
module ALife.Creatur.Checklist
  (
    Checklist(..),
    PersistentChecklist,
    mkPersistentChecklist
  ) where

import ALife.Creatur.Util (modifyLift)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, gets, put)
import qualified Data.List as L
import System.Directory (doesFileExist)
import System.IO (hGetContents, withFile, Handle, IOMode(ReadMode))
import Text.Read (readEither)

type Status = ([String], [String]) -- (toDo, done)

class Checklist t where
  status :: StateT t IO Status
  markDone :: String -> StateT t IO ()
  done :: StateT t IO Bool
  setItems :: [String] -> StateT t IO ()
  delete :: String -> StateT t IO ()

data PersistentChecklist = PersistentChecklist {
    tInitialised :: Bool,
    tStatus :: Status,
    tFilename :: FilePath
  } deriving Show

-- | Creates a counter that will store its value in the specified file.
mkPersistentChecklist :: FilePath -> PersistentChecklist
mkPersistentChecklist f = PersistentChecklist False ([],[]) f

instance Checklist PersistentChecklist where
  status = initIfNeeded >> gets tStatus
  markDone x = do
    t <- get
    let (ys,zs) = tStatus t
    when (x `elem` ys) $ do
      let t' = t { tStatus=(L.delete x ys,zs ++ [x]) }
      put t'
      liftIO $ store t'
  done = gets (null . fst . tStatus)
  setItems ts = do
    t <- get
    let t' = t { tStatus=(ts,[]) }
    put t'
    liftIO $ store t'
  delete tOld = do
    t <- get
    let (xs,ys) = tStatus t
    let t' = t { tStatus=(L.delete tOld xs,L.delete tOld ys) } 
    put t'
    liftIO $ store t'

initIfNeeded :: StateT PersistentChecklist IO ()
initIfNeeded = do
  isInitialised <- gets tInitialised
  unless isInitialised $ modifyLift initialise

initialise :: PersistentChecklist -> IO PersistentChecklist
initialise t = do
  let f = tFilename t
  fExists <- doesFileExist f
  if fExists
    then do
      s <- withFile f ReadMode readChecklist -- closes file ASAP
      case s of
        Left msg ->
          error $ "Unable to read checklist from " ++ f ++ ": " ++ msg
        Right s'  -> return $ t { tInitialised=True, tStatus=s' }
    else return $ t { tInitialised=True }

readChecklist :: Handle -> IO (Either String Status)
readChecklist h = do
  x <- hGetContents h
  let s = readEither x
  case s of
    Left msg -> return $ Left (msg ++ "\"" ++ x ++ "\"")
    Right c  -> return $ Right c

store :: PersistentChecklist -> IO ()
store t = writeFile (tFilename t) $ show (tStatus t)
