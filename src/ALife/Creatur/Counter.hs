------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Counter
-- Copyright   :  (c) Amy de Buitléir 2012-2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple counter which persists between runs.
--
------------------------------------------------------------------------
module ALife.Creatur.Counter
  (
    Counter(..),
    PersistentCounter,
    mkPersistentCounter
  ) where

import ALife.Creatur.Clock (Clock, currentTime, incTime)
import ALife.Creatur.Util (modifyLift)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, gets, modify)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (dropFileName)
import System.IO (hGetContents, withFile, Handle, IOMode(ReadMode))
import Text.Read (readEither)

class Counter c where
  current :: StateT c IO Int
  increment :: StateT c IO ()

data PersistentCounter = PersistentCounter {
    cInitialised :: Bool,
    cValue :: Int,
    cFilename :: FilePath
  } deriving (Show, Eq)

-- | Creates a counter that will store its value in the specified file.
mkPersistentCounter :: FilePath -> PersistentCounter
mkPersistentCounter = PersistentCounter False (-1)

instance Counter PersistentCounter where
  current = initIfNeeded >> gets cValue
  increment = do
    initIfNeeded
    modify (\c -> c { cValue=1 + cValue c })
    k <- get
    liftIO $ store k

store :: PersistentCounter -> IO ()
store counter = do
  let f = cFilename counter
  createDirectoryIfMissing True $ dropFileName f
  writeFile f $ show (cValue counter)
  
initIfNeeded :: StateT PersistentCounter IO ()
initIfNeeded = do
  isInitialised <- gets cInitialised
  unless isInitialised $ modifyLift initialise

initialise :: PersistentCounter -> IO PersistentCounter
initialise counter = do
  let f = cFilename counter
  fExists <- doesFileExist f
  if fExists
    then do
      x <- withFile f ReadMode readCounter -- closes file ASAP
      case x of
        Left msg -> error $ "Unable to read counter from " ++ f ++ ": " ++ msg
        Right c  -> return $ counter { cInitialised=True, cValue=c }
    else do
      let k = counter { cInitialised=True, cValue=0 }
      return k

instance Clock PersistentCounter where
  currentTime = current
  incTime = increment

readCounter :: Handle -> IO (Either String Int)
readCounter h = do
  s <- hGetContents h
  let x = readEither s
  case x of
    Left msg -> return $ Left (msg ++ "\"" ++ s ++ "\"")
    Right c  -> return $ Right c
