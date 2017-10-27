------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Persistent
-- Copyright   :  (c) Amy de Buitléir 2012-2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A state which persists between runs.
--
------------------------------------------------------------------------
module ALife.Creatur.Persistent
  (
    Persistent,
    mkPersistent,
    getPS,
    putPS,
    modifyPS,
    runPS
  ) where

import ALife.Creatur.Util (modifyLift)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, gets, modify)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (dropFileName)
import System.IO (hGetContents, withFile, Handle, IOMode(ReadMode))
import Text.Read (readEither)

data Persistent a = Persistent {
    psInitialised :: Bool,
    psValue :: a,
    psDefaultValue :: a,
    psFilename :: FilePath
  } deriving (Show, Read, Eq)

-- | Creates a counter that will store its value in the specified file.
mkPersistent :: a -> FilePath -> Persistent a
mkPersistent s = Persistent False s s

getPS :: Read a => StateT (Persistent a) IO a
getPS = initIfNeeded >> gets psValue

putPS :: (Show a, Read a) => a -> StateT (Persistent a) IO ()
putPS s = do
  initIfNeeded
  modify (\p -> p { psValue=s })
  p' <- get
  liftIO $ store p'

modifyPS :: (Show a, Read a) => (a -> a) -> StateT (Persistent a) IO ()
modifyPS f = do
  p <- getPS
  putPS $ f p

runPS :: Read a => (a -> b) -> StateT (Persistent a) IO b
runPS f = do
  p <- getPS
  return $ f p

store :: Show a => Persistent a -> IO ()
store p = do
  let f = psFilename p
  createDirectoryIfMissing True $ dropFileName f
  writeFile f $ show (psValue p)

initIfNeeded :: Read a => StateT (Persistent a) IO ()
initIfNeeded = do
  isInitialised <- gets psInitialised
  unless isInitialised $ modifyLift initialise

initialise :: Read a => Persistent a -> IO (Persistent a)
initialise p = do
  let f = psFilename p
  fExists <- doesFileExist f
  if fExists
    then do
      x <- withFile f ReadMode readValue -- closes file ASAP
      case x of
        Left msg -> error $ "Unable to read value from " ++ f ++ ": " ++ msg
        Right c  -> return $ p { psInitialised=True, psValue=c }
    else do
      return $ p { psInitialised=True, psValue=psDefaultValue p }

readValue :: Read a => Handle -> IO (Either String a)
readValue h = do
  s <- hGetContents h
  let x = readEither s
  case x of
    Left msg -> return $ Left (msg ++ "\"" ++ s ++ "\"")
    Right c  -> return $ Right c
