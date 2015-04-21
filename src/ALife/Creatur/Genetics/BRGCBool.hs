------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.BRGCBool
-- Copyright   :  (c) Amy de Buitléir 2013-2015
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with genes that are encoded as a sequence of
-- bits, using a Binary Reflected Gray Code (BRGC).
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
module ALife.Creatur.Genetics.BRGCBool
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
    consumed2
  ) where

import Prelude hiding (read)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Util (fromEither)
import Codec.Gray (integralToGray, grayToIntegral)
import Control.Monad (replicateM)
import Control.Monad.State.Lazy (StateT, runState, execState, evalState)
import qualified Control.Monad.State.Lazy as S (put, get, gets)
import Data.Char (ord, chr, intToDigit)
import Data.Functor.Identity (Identity)
import Data.Word (Word8, Word16)
import GHC.Generics
import Numeric (showIntAtBase)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif

type Sequence = [Bool]

type Writer = StateT Sequence Identity

write :: Genetic x => x -> Sequence
write x = execState (put x) []

runWriter :: Writer () -> Sequence
runWriter w = execState w []

type Reader = StateT (Sequence, Int) Identity

read :: Genetic g => Sequence -> Either [String] g
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
  get :: Reader (Either [String] g)

  default get :: (Generic g, GGenetic (Rep g)) => Reader (Either [String] g)
  get = do
    a <- gget
    return $ fmap to a

  getWithDefault :: g -> Reader g
  getWithDefault d = fmap (fromEither d) get

class GGenetic f where
  gput :: f a -> Writer ()
  gget :: Reader (Either [String] (f a))

-- | Unit: used for constructors without arguments
instance GGenetic U1 where
  gput U1 = return ()
  gget = return (Right U1)

-- | Constants, additional parameters and recursion of kind *
instance (GGenetic a, GGenetic b) => GGenetic (a :*: b) where
  gput (a :*: b) = gput a >> gput b
  gget = do
    a <- gget
    b <- gget
    return $ (:*:) <$> a <*> b

-- | Meta-information (constructor names, etc.)
instance (GGenetic a, GGenetic b) => GGenetic (a :+: b) where
  gput (L1 x) = put True >> gput x
  gput (R1 x) = put False >> gput x
  gget = do
    a <- get
    case a of
      Right True  -> fmap (fmap L1) gget
      Right False -> fmap (fmap R1) gget
      Left s -> return (Left s)

-- | Sums: encode choice between constructors
instance (GGenetic a) => GGenetic (M1 i c a) where
  gput (M1 x) = gput x
  gget = fmap (fmap M1) gget

-- | Products: encode multiple arguments to constructors
instance (Genetic a) => GGenetic (K1 i a) where
  gput (K1 x) = put x
  gget = do
    a <- get
    return $ fmap K1 a

--
-- Instances
--

instance Genetic Bool where
  put x = do
    xs <- S.get
    S.put (xs ++ [x])
  get = do
    (xs, i) <- S.get
    let xs' = drop i xs
    if null xs'
       then return $ Left ["End of sequence"]
       else do
         let x = head xs'
         S.put (xs, i+1)
         return $ Right x

instance Genetic Char where
  put = putRawBoolArray . intToBools 8 . ord
  get = do
    bs <- getRawBoolArray 8
    return . fmap chr $ fmap boolsToInt bs

instance Genetic Word8 where
  put = putRawBoolArray . intToBools 8 . integralToGray
  get = fmap (fmap (grayToIntegral . boolsToInt)) (getRawBoolArray 8)

instance Genetic Word16 where
  put = putRawBoolArray . intToBools 16 . integralToGray
  get = fmap (fmap (grayToIntegral . boolsToInt)) (getRawBoolArray 16)

instance (Genetic a) => Genetic [a]

instance (Genetic a) => Genetic (Maybe a)

instance (Genetic a, Genetic b) => Genetic (a, b)

instance (Genetic a, Genetic b) => Genetic (Either a b)


--
-- Utilities
--

-- Useful when we know exactly how many bits there should be.
putRawBoolArray :: [Bool] -> Writer ()
putRawBoolArray = mapM_ put

getRawBoolArray :: Int -> Reader (Either [String] [Bool])
getRawBoolArray n = do
  xs <- replicateM n get
  return $ sequence xs

intToBools :: (Integral a, Show a) => Int -> a -> [Bool]
intToBools nBits x =
  map (\b -> b == '1') . tail $ showIntAtBase 2 intToDigit x' ""
  where x' = 2^nBits + fromIntegral x :: Int

boolsToInt :: Integral a => [Bool] -> a
boolsToInt bs = f 0 ns
  where ns = map (\x -> if x then 1 else 0) bs
        f i [] = i
        f i (j:js) = f (i*2+j) js

--
-- Diploid genes
--

type DiploidSequence = (Sequence, Sequence)

type DiploidReader = StateT ((Sequence, Int), (Sequence, Int)) Identity

readAndExpress :: (Genetic g, Diploid g) => DiploidSequence -> Either [String] g
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
getAndExpress :: (Genetic g, Diploid g) => DiploidReader (Either [String] g)
getAndExpress = do
  (sa, sb) <- S.get
  let (a, sa') = runState get sa
  let (b, sb') = runState get sb
  S.put (sa', sb')
  return $ expressEither a b

getAndExpressWithDefault :: (Genetic g, Diploid g) => g -> DiploidReader g
getAndExpressWithDefault d = fmap (fromEither d) getAndExpress

expressEither
  :: Diploid g
    => Either [String] g -> Either [String] g
      -> Either [String] g
expressEither (Right a) (Right b) = Right (express a b)
expressEither (Right a) (Left _)  = Right a
expressEither (Left _)  (Right b) = Right b
expressEither (Left xs) (Left ys) =
  Left $ (map ("sequence 1: " ++) xs) ++ (map ("sequence 2: " ++) ys)
