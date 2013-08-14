------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.BRGCWord8
-- Copyright   :  (c) Amy de Buitléir 2013
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with genes that are encoded as a sequence of
-- bytes, using a Binary Reflected Gray Code (BRGC).
--
-- A Gray code maps values to codes in a way that guarantees that the
-- codes for two consecutive values will differ by only one bit. This
-- feature can be useful in evolutionary programming because the genes
-- resulting from a crossover operation are likely to be similar to
-- the inputs. This helps to ensure that offspring are similar to
-- their parents, as any radical changes from one generation to the
-- next are the result of mutation alone.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances,
    DefaultSignatures, DeriveGeneric, TypeOperators #-}
module ALife.Creatur.Genetics.BRGCWord8
  (
    Genetic(..),
    Sequence,
    Writer,
    write,
    runWriter,
    Reader,
    read,
    runReader,
    copy,
    consumed,
    DiploidSequence,
    DiploidReader,
    readAndExpress,
    runDiploidReader,
    getAndExpress,
    getAndExpressWithDefault,
    copy2,
    consumed2,
    putRawWord8,
    getRawWord8
  ) where

import Prelude hiding (read)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import Codec.Gray (integralToGray, grayToIntegral)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State.Lazy (StateT, runState, execState, evalState)
import qualified Control.Monad.State.Lazy as S (put, get, gets)
import Data.Char (ord, chr)
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe)
import Data.Word (Word8, Word16)
import GHC.Generics

type Sequence = [Word8]

type Writer = StateT Sequence Identity

write :: Genetic x => x -> Sequence
write x = execState (put x) []

runWriter :: Writer () -> Sequence
runWriter w = execState w []

type Reader = StateT (Sequence, Int) Identity

read :: Genetic g => Sequence -> Maybe g
read s = evalState get (s, 0)

runReader :: Reader g -> Sequence -> g
runReader r s = evalState r (s, 0)

-- | Return the entire genome.
copy :: Reader Sequence
copy = S.gets fst

-- | Return the portion of the genome that has been read.
consumed :: Reader Sequence
consumed = do
  (xs, i) <- S.get
  return $ take i xs

-- | A class representing anything which is represented in, and
--   determined by, an agent's genome.
--   This might include traits, parameters, "organs" (components of
--   agents), or even entire agents.
--   Instances of this class can be thought of as genes, i.e.,
--   instructions for building an agent.
class Genetic g where
  -- | Writes a gene to a sequence.
  put :: g -> Writer ()

  default put :: (Generic g, GGenetic (Rep g)) => g -> Writer ()
  put = gput . from

  -- | Reads the next gene in a sequence.
  get :: Reader (Maybe g)

  default get :: (Generic g, GGenetic (Rep g)) => Reader (Maybe g)
  get = do
    a <- gget
    return . fmap to $ a

  getWithDefault :: g -> Reader g
  getWithDefault d = fmap (fromMaybe d) get

class GGenetic f where
  gput :: f a -> Writer ()
  gget :: Reader (Maybe (f a))

-- | Unit: used for constructors without arguments
instance GGenetic U1 where
  gput U1 = return ()
  gget = return (Just U1)

-- | Constants, additional parameters and recursion of kind *
instance (GGenetic a, GGenetic b) => GGenetic (a :*: b) where
  gput (a :*: b) = gput a >> gput b
  gget = do
    a <- gget
    b <- gget
    return $ (:*:) <$> a <*> b

-- | Meta-information (constructor names, etc.)
instance (GGenetic a, GGenetic b) => GGenetic (a :+: b) where
  gput (L1 x) = putRawWord8 0 >> gput x
  gput (R1 x) = putRawWord8 1 >> gput x
  gget = do
    a <- getRawWord8
    case a of
      (Just x) -> do
        if even x -- Only care about the last bit
          then fmap (fmap L1) gget
          else fmap (fmap R1) gget
      Nothing -> return Nothing

-- | Sums: encode choice between constructors
instance (GGenetic a) => GGenetic (M1 i c a) where
  gput (M1 x) = gput x
  gget = fmap (fmap M1) gget

-- | Products: encode multiple arguments to constructors
instance (Genetic a) => GGenetic (K1 i a) where
  gput (K1 x) = put x
  gget = do
    a <- get
    return . fmap K1 $ a

--
-- Instances
--

instance Genetic Bool where
  put False = putRawWord8 0
  put True  = putRawWord8 1
  get = fmap (fmap word8ToBool) getRawWord8

word8ToBool :: Word8 -> Bool
word8ToBool x = if even x then False else True

instance Genetic Char where
  put = putRawWord8 . fromIntegral . ord
  get = fmap (fmap (chr . fromIntegral)) getRawWord8

instance Genetic Word8 where
  put = putRawWord8 . integralToGray
  get = fmap (fmap grayToIntegral) getRawWord8

instance Genetic Word16 where
  put g = putRawWord8 high >> putRawWord8 low
    where x = integralToGray g
          high = fromIntegral (x `div` 0x100)
          low = fromIntegral (x `mod` 0x100)
  get = do
    h <- getRawWord8 :: Reader (Maybe Word8)
    let high = fmap (\x -> fromIntegral x * 0x100) h :: Maybe Word16
    l <- getRawWord8 :: Reader (Maybe Word8)
    let low = fmap fromIntegral l :: Maybe Word16
    return . fmap grayToIntegral $ (+) <$> high <*> low

instance (Genetic a) => Genetic [a]

instance (Genetic a) => Genetic (Maybe a)

--
-- Utilities
--

-- | Write a Word8 value to the genome without encoding it
putRawWord8 :: Word8 -> Writer ()
putRawWord8 x = do
  xs <- S.get
  S.put (xs ++ [x])

-- | Read a Word8 value from the genome without decoding it
getRawWord8 :: Reader (Maybe Word8)
getRawWord8 = do
  (xs, i) <- S.get
  let xs' = drop i xs
  if null xs'
     then return Nothing
     else do
       let x = head xs'
       S.put (xs, i+1)
       return $ Just x

--
-- Diploid genes
--

type DiploidSequence = (Sequence, Sequence)

type DiploidReader = StateT ((Sequence, Int), (Sequence, Int)) Identity

readAndExpress :: (Genetic g, Diploid g) => DiploidSequence -> Maybe g
readAndExpress (s1, s2) = evalState getAndExpress ((s1, 0), (s2, 0))

runDiploidReader :: DiploidReader g -> DiploidSequence -> g
runDiploidReader r (s1, s2) = evalState r ((s1, 0), (s2, 0))

-- | Return the entire genome.
copy2 :: DiploidReader DiploidSequence
copy2 = do
  (ra, rb) <- S.get
  let as = evalState copy ra
  let bs = evalState copy rb
  return (as, bs)

-- | Return the portion of the genome that has been read.
consumed2 :: DiploidReader DiploidSequence
consumed2 = do
  (ra, rb) <- S.get
  let as = evalState consumed ra
  let bs = evalState consumed rb
  return (as, bs)

-- | Read the next pair of genes from twin sequences of genetic
--   information, and return the resulting gene (after taking
--   into account any dominance relationship) and the remaining
--   (unread) portion of the two nucleotide strands.
getAndExpress :: (Genetic g, Diploid g) => DiploidReader (Maybe g)
getAndExpress = do
  (sa, sb) <- S.get
  let (a, sa') = runState get sa
  let (b, sb') = runState get sb
  S.put (sa', sb')
  return $ expressMaybe a b

getAndExpressWithDefault :: (Genetic g, Diploid g) => g -> DiploidReader g
getAndExpressWithDefault d = fmap (fromMaybe d) getAndExpress

expressMaybe :: Diploid g => Maybe g -> Maybe g -> Maybe g
expressMaybe (Just a) (Just b) = Just (express a b)
expressMaybe (Just a) Nothing  = Just a
expressMaybe Nothing (Just b)  = Just b
expressMaybe Nothing Nothing   = Nothing
