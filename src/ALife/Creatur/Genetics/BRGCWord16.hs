------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.BRGCWord16
-- Copyright   :  (c) Amy de Buitléir 2014-2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with genes that are encoded as a sequence of
-- 16-bit words, using a Binary Reflected Gray Code (BRGC).
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
module ALife.Creatur.Genetics.BRGCWord16
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
    putRawWord16,
    getRawWord16,
    putRawWord16s,
    getRawWord16s
  ) where

import Prelude hiding (read)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Util (fromEither)
import Codec.Gray (integralToGray, grayToIntegral)
import Control.Monad.State.Lazy (StateT, runState, execState, evalState)
import qualified Control.Monad.State.Lazy as S (put, get, gets)
import Data.Binary (Binary, encode, decode)
import Data.ByteString.Lazy (pack, unpack)
import Data.Char (ord, chr)
import Data.Functor.Identity (Identity)
import Data.Word (Word8, Word16)
import GHC.Generics

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif

type Sequence = [Word16]

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

  getWithName :: String -> Reader (Either [String] g)
  getWithName s = do
    g0 <- get
    return $ case g0 of
               (Left xs) -> Left ((s ++ ":"):xs)
               (Right g1) -> Right g1

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
  gput (L1 x) = putRawWord16 0 >> gput x
  gput (R1 x) = putRawWord16 1 >> gput x
  gget = do
    a <- getRawWord16
    case a of
      Right x -> do
        if even x -- Only care about the last bit
          then fmap (fmap L1) gget
          else fmap (fmap R1) gget
      Left s -> return $ Left s

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
  put False = putRawWord16 0
  put True  = putRawWord16 1
  get = fmap (fmap word16ToBool) getRawWord16

word16ToBool :: Word16 -> Bool
word16ToBool x = if even x then False else True

instance Genetic Char where
  put = put . ord
  get = fmap (fmap chr) get

instance Genetic Word8 where
  put x = put (fromIntegral x :: Word16)
  get = do
    x <- get :: Reader (Either [String] Word16)
    return $ fmap fromIntegral x

instance Genetic Word16 where
  put = putRawWord16 . integralToGray
  get = fmap (fmap grayToIntegral) getRawWord16

instance Genetic Int where
  put g = put (map integralToGray . integralToByteArray $ g)
  get = do
    x <- get
    case x of
      Right xs -> return $ Right (byteArrayToIntegral . map grayToIntegral $ xs)
      Left s   -> return $ Left s

instance (Genetic a) => Genetic [a]

instance (Genetic a) => Genetic (Maybe a)

instance (Genetic a, Genetic b) => Genetic (a, b)

instance (Genetic a, Genetic b) => Genetic (Either a b)


--
-- Utilities
--

integralToByteArray :: (Integral t, Binary t) => t -> [Word8]
integralToByteArray = unpack . encode

byteArrayToIntegral :: (Integral t, Binary t) => [Word8] -> t
byteArrayToIntegral = decode . pack

-- | Write a Word16 value to the genome without encoding it
putRawWord16 :: Word16 -> Writer ()
putRawWord16 x = do
  xs <- S.get
  S.put (xs ++ [x])

-- | Read a Word16 value from the genome without decoding it
getRawWord16 :: Reader (Either [String] Word16)
getRawWord16 = do
  (xs, i) <- S.get
  let xs' = drop i xs
  if null xs'
     then return $ Left ["End of sequence"]
     else do
       let x = head xs'
       S.put (xs, i+1)
       return $ Right x

-- | Write a raw sequence of Word16 values to the genome
putRawWord16s :: [Word16] -> Writer ()
putRawWord16s ys = do
  xs <- S.get
  S.put (xs ++ ys)

-- | Read a raw sequence of Word16 values from the genome
getRawWord16s :: Int -> Reader (Either [String] [Word16])
getRawWord16s n =
  if n == 0
    then return $ Right []
    else do
      (xs, i) <- S.get
      let xs' = drop i xs
      if null xs' || length xs' < n
        then return $ Left ["End of genes"]
        else do
          let ys = take n xs'
          S.put (xs, i+n)
          return $ Right ys

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
