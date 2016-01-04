------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.BRGCWord8
-- Copyright   :  (c) Amy de Buitléir 2013-2015
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
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
    putAndReport,
    getAndReport,
    putRawWord8,
    getRawWord8,
    putRawWord8s,
    getRawWord8s
  ) where

import Prelude hiding (read)
import ALife.Creatur.Genetics.Diploid (Diploid, express)
import ALife.Creatur.Util (fromEither)
import Codec.Gray (integralToGray, grayToIntegral)
import Control.Monad (replicateM)
import Control.Monad.State.Lazy (StateT, runState, execState, evalState)
import qualified Control.Monad.State.Lazy as S (put, get, gets)
import Data.Char (ord, chr)
import Data.Either (partitionEithers)
import Data.Functor.Identity (Identity)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
#endif

type Sequence = [Word8]

type Writer = StateT (Sequence, [String]) Identity

write :: Genetic x => x -> Sequence
write x = fst $ runWriter (put x)

runWriter :: Writer () -> (Sequence, [String])
runWriter w = execState (w >> finalise) ([], [])

type Reader = StateT (Sequence, Int, [String]) Identity

read :: Genetic g => Sequence -> Either [String] g
read s = fst $ runReader get s

runReader
  :: Reader (Either [String] g) -> Sequence
    -> (Either [String] g, [String])
runReader r s = (x, reverse msgs)
  where (x, (_, _, msgs)) = runState r (s, 0, [])

-- | Return the entire genome.
copy :: Reader Sequence
copy = S.gets (\(x, _, _) -> x)

-- | Return the portion of the genome that has been read.
consumed :: Reader Sequence
consumed = do
  (xs, i, _) <- S.get
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
  gput (L1 x) = putAndReport [0] "L1" >> gput x
  gput (R1 x) = putAndReport [1] "R1" >> gput x
  gget = do
    a <- getAndReport 1 convertLR
    case a of
      Right L -> fmap (fmap L1) gget
      Right R -> fmap (fmap R1) gget
      Left s  -> return $ Left s

data LR = L | R

convertLR :: [Word8] -> Either String (LR, String)
convertLR (x:[]) = if even x -- Only care about the last bit
                     then Right (L, "L1")
                     else Right (R, "R1")
convertLR _ = Left "logic error"
  
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
  put b = putAndReport [fromIntegral $ fromEnum b] (show b)
  get = getAndReport 1 convert
    where convert (x:[]) = Right (g, show g)
            where g = word8ToBool x
          convert _ = Left "logic error"

word8ToBool :: Word8 -> Bool
word8ToBool x = if even x then False else True

instance Genetic Char where
  put x = putAndReport [fromIntegral . ord $ x] (show x)
  get = getAndReport 1 convert
    where convert (x:[]) = Right (g, show g)
            where g = (chr . fromIntegral) x
          convert _ = Left "logic error"

instance Genetic Word8 where
  put x = putAndReport [integralToGray x] (show x ++ " Word8")
  get = getAndReport 1 convert
    where convert (x:[]) = Right (g, show g ++ " Word8")
            where g = grayToIntegral x
          convert _ = Left "logic error"

instance Genetic Word16 where
  put g = putAndReport (integralToBytes 2 x) (show g ++ " Word16")
    where x = integralToGray g
  get = getAndReport 2 grayWord16

instance Genetic Word32 where
  put g = putAndReport (integralToBytes 4 x) (show g ++ " Word32")
    where x = integralToGray g
  get = getAndReport 4 grayWord32

instance Genetic Word64 where
  put g = putAndReport (integralToBytes 8 x) (show g ++ " Word64")
    where x = integralToGray g
  get = getAndReport 8 grayWord64

grayWord16 :: [Word8] -> Either String (Word16, String)
grayWord16 bs = Right (g, show g ++ " Word16")
  where g = grayToIntegral . bytesToIntegral $ bs

grayWord32 :: [Word8] -> Either String (Word32, String)
grayWord32 bs = Right (g, show g ++ " Word32")
  where g = grayToIntegral . bytesToIntegral $ bs

grayWord64 :: [Word8] -> Either String (Word64, String)
grayWord64 bs = Right (g, show g ++ " Word64")
  where g = grayToIntegral . bytesToIntegral $ bs

integralToBytes :: Integral t => Int -> t -> [Word8]
integralToBytes n x = f n x []
  where f 0 _ bs = bs
        f m y bs = f (m-1) y' (b:bs)
          where y' = y `div` 0x100
                b = fromIntegral $ y `mod` 0x100
 
bytesToIntegral :: Integral t => [Word8] -> t
bytesToIntegral bs = f (bs, 0)
  where f ([], n) = n
        f (k:ks, n) = f (ks, n*0x100 + fromIntegral k)

instance (Genetic a) => Genetic [a] where
  put xs = do
    put n'
    replaceReportW (show n' ++ " list length")
    mapM_ put xs
    where n = length xs
          n' = if n <= fromIntegral (maxBound :: Word16)
                 then fromIntegral n
                 else error "List too long" :: Word16
  get = do
    n <- get :: Reader (Either [String] Word16)
    case n of
      Right n' -> do replaceReportR (show n' ++ " list length")
                     getList (fromIntegral n')
      Left s   -> return $ Left s

instance (Genetic a) => Genetic (Maybe a)

instance (Genetic a, Genetic b) => Genetic (a, b)

instance (Genetic a, Genetic b) => Genetic (Either a b)


--
-- Utilities
--

finalise :: Writer ()
finalise = do
  (xs, msgs) <- S.get
  S.put (reverse xs, reverse msgs)

getList :: Genetic a => Int -> Reader (Either [String] [a])
getList 0 = return $ Right []
getList n = do
  cs <- sequence $ replicate n get
  let (mss, xs) = partitionEithers cs
  if null mss
    then return $ Right xs
    else return $ Left (head mss)
 
-- | Write a Word8 value to the genome without encoding it
putRawWord8 :: Word8 -> Writer ()
putRawWord8 x = do
  (xs, msgs) <- S.get
  S.put (x:xs, msgs)

-- | Read a Word8 value from the genome without decoding it
getRawWord8 :: Reader (Either [String] Word8)
getRawWord8 = do
  (xs, i, msgs) <- S.get
  let xs' = drop i xs
  if null xs'
     then return $ Left ["End of sequence"]
     else do
       let x = head xs'
       S.put (xs, i+1, msgs)
       return $ Right x

-- | Write a raw sequence of Word8 values to the genome
putRawWord8s :: [Word8] -> Writer ()
putRawWord8s ws = mapM_ putRawWord8 ws

-- | Read a raw sequence of Word8 values from the genome
getRawWord8s :: Int -> Reader (Either [String] [Word8])
getRawWord8s n = fmap sequence $ replicateM n getRawWord8

reportW :: String -> Writer ()
reportW desc = do
  (xs, msgs) <- S.get
  let msg = show (length xs) ++ ": wrote " ++ desc
  S.put (xs, msg:msgs)
  
putAndReport :: [Word8] -> String -> Writer ()
putAndReport bytes msg = putRawWord8s bytes >> reportW msg

replaceReportW :: String -> Writer ()
replaceReportW desc = do
  (xs, _:msgs) <- S.get
  let msg = show (length xs) ++ ": wrote " ++ desc
  S.put (xs, msg:msgs)
  
reportR :: String -> Reader ()
reportR desc = do
  (xs, i, msgs) <- S.get
  let msg = show i ++ ": read " ++ desc
  S.put (xs, i, msg:msgs)

getAndReport :: Int -> ([Word8] -> (Either String (g, String))) -> Reader (Either [String] g)
getAndReport n parse = do
  a <- getRawWord8s n
  case a of
   Right xs    -> case parse xs of
                    Right (g, msg) -> reportR msg >> return (Right g)
                    Left errMsg2   -> return $ Left [errMsg2]
   Left errMsg -> return $ Left errMsg

replaceReportR :: String -> Reader ()
replaceReportR desc = do
  (xs, i, _:msgs) <- S.get
  let msg = show i ++ ": read " ++ desc
  S.put (xs, i, msg:msgs)
  
--
-- Diploid genes
--

type DiploidSequence = (Sequence, Sequence)

type DiploidReader = StateT ((Sequence, Int, [String]), (Sequence, Int, [String])) Identity

readAndExpress :: (Genetic g, Diploid g) => DiploidSequence -> Either [String] g
readAndExpress (s1, s2) = evalState getAndExpress ((s1, 0, []), (s2, 0, []))

runDiploidReader :: DiploidReader g -> DiploidSequence -> g
runDiploidReader r (s1, s2) = evalState r ((s1, 0, []), (s2, 0, []))

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
