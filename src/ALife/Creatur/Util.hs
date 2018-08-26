------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Util
-- Copyright   :  (c) Amy de Buitléir 2011-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions that don't fit anywhere else.
--
------------------------------------------------------------------------
module ALife.Creatur.Util
  (
    -- * Integers
    ilogBase,
    isPowerOf,
    isqrt,
    perfectSquare,
    -- * Arrays
    cropRect,
    cropSquare,
    -- * Sequences
    replaceElement,
    reverseLookup,
    rotate,
    safeReplaceElement,
    shuffle,
    -- * Bits/Booleans
    boolsToBits,
    showBin,
    -- * Monads
    stateMap,
    fromEither,
    catEithers,
    modifyLift,
    getLift
--    constrain,
  ) where

import Control.Monad (forM_, liftM)
import Control.Monad.Random (Rand, RandomGen, getRandomRs)
import Control.Monad.State (StateT(..), get, lift, put)
import Data.Array.ST (runSTArray)
import Data.Char (intToDigit)
import Data.List.Split (chunksOf)
import GHC.Arr (elems, listArray, readSTArray, thawSTArray, writeSTArray)
import Numeric (showIntAtBase)

-- constrain :: Ord a => (a, a) -> a -> a
-- constrain (a,b) x | b < a     = error "Invalid range"
--                   | x < a     = a
--                   | x > b     = b
--                   | otherwise = x

-- | From <http://www.haskell.org/haskellwiki/Random_shuffle>
shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle xs = do
  let l = length xs
  rands <- take l `fmap` getRandomRs (0, l-1)
  let ar = runSTArray $ do
             ar' <- thawSTArray $ listArray (0, l-1) xs
             forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
               vi <- readSTArray ar' i
               vj <- readSTArray ar' j
               writeSTArray ar' j vi
               writeSTArray ar' i vj
             return ar'
  return (elems ar)

-- | @'safeReplaceElement' xs n x@ returns a copy of @xs@ in which the @n@th
--   element (if it exists) has been replaced with @x@.
safeReplaceElement :: [a] -> Int -> a -> [a]
safeReplaceElement xs i x =
  if i >= 0 && i < length xs
    then replaceElement xs i x
    else xs

-- | @'replaceElement' xs n x@ returns a copy of @xs@ in which the @n@th
--   element has been replaced with @x@. Causes an exception if @xs@ has
--   fewer than @n+1@ elements. Compare with @'safeReplaceElement'@.
replaceElement :: [a] -> Int -> a -> [a]
replaceElement xs i x =
  if 0 <= i && i < length xs then fore ++ (x : aft) else xs
  where fore = take i xs
        aft = drop (i+1) xs

-- | Assuming @xs@ is a sequence containing the elements of a square matrix,
--   @'cropSquare' n xs@ returns the elements of the submatrix of size @n@x@n@,
--   centred within the original matrix @xs@.
--
--   Example: Suppose we have a /5/x/5/ matrix and we want to extract the
--   central /3/x/3/ submatrix, as illustrated below.
--
-- > a b c d e
-- > f g h i j            g h i
-- > k l m n o    --->    l m n
-- > p q r s t            q r s
-- > u v w x y
--
--   We can represent the elements of the original matrix as @[\'a\'..\'y\']@.
--   The elements of the submatrix are
--   @[\'g\', \'h\', \'i\', \'l\', \'m\', \'n\', \'q\', \'r\', \'s\']@,
--   or equivalently, @\"ghilmnqrs\"@. And that is what
--   @'cropSquare' 3 [\'a\'..\'y\']@ returns.
cropSquare :: Int -> [a] -> [a]
cropSquare n xs | n <= 0     = []
                | n < m     =
                    cropRect (margin, margin) (margin+n-1, margin+n-1) xs m
                | otherwise = take (m*m) xs
  where m = (isqrt . length) xs
        margin = (m - n) `div` 2

-- | Assuming @xs@ is a sequence containing the elements of a matrix with @k@
--   columns, @'cropRect' (a,b) (c, d) k xs@ returns the elements of the
--   submatrix from @(a,b)@ in the upper left corner to @(c,d)@ in the lower
--   right corner).
--   Note: Matrix indices begin at @(0,0)@.
--
--   Example: Suppose we have a /4/x/6/ matrix and we want to extract the
--   submatrix from (1,2) to (2,4), as illustrated below.
--
-- > a b c d e f
-- > g h i j k l    --->   i j k
-- > m n o p q r           o p q
-- > s t u v w x
--
--   We can represent the elements of the original matrix as @[\'a\'..\'x\']@.
--   The elements of the submatrix are
--   @[\'i\', \'j\', \'k\', \'o\', \'p\', \'q\']@, or equivalently,
--   @\"ijkopq\"@. And that is what @'cropRect' (1,2) (2,4) 6 [\'a\'..\'x\']@
--   returns.
cropRect :: (Int, Int) -> (Int, Int) -> [a] -> Int -> [a]
cropRect (a,b) (c, d) xs k = concatMap f selectedRows
  where rows = if k <= 0 then [] else chunksOf k xs
        selectedRows = safeSlice a c rows
        f = safeSlice b d

safeSlice :: Int -> Int -> [a] -> [a]
safeSlice a b = drop a . take (b+1)

-- | @'isqrt' n@ returns the greatest integer not greater than the square root
--   of @n@.
isqrt :: (Integral a, Integral b) => a -> b
isqrt n = (floor . sqrt) n'
  where n' = fromIntegral n :: Float

-- | @'ilogBase' n m@ returns the greatest integer not greater than the log
--   base n of @m@.
ilogBase :: (Integral a, Integral b, Integral c) => a -> b -> c
ilogBase n m = (floor . logBase n') m'
  where n' = fromIntegral n :: Float
        m' = fromIntegral m :: Float

-- | @'perfectSquare' n@ returns @True@ if @n@ is a perfect square (i.e., if
--   there exists an _integer_ m such that m*m = n)
perfectSquare :: Integral a => a -> Bool
perfectSquare n = n == m*m
  where m = isqrt n

-- | @n 'isPowerOf' m@ returns @True@ if @n@ is a power of m (i.e., if
--   there exists an _integer_ k such that m^k = n)
isPowerOf :: Integral a => a -> a -> Bool
isPowerOf n m = n == m^k
  where k = ilogBase m n :: Int

reverseLookup :: (Eq b) => b -> [(a,b)] -> Maybe a
reverseLookup _ []          =  Nothing
reverseLookup value ((x,y):xys)
    | value == y =  Just x
    | otherwise  =  reverseLookup value xys

stateMap :: Monad m => (s -> t) -> (t -> s) -> StateT s m a -> StateT t m a
stateMap f g (StateT h) = StateT $ liftM (fmap f) . h . g

-- | The 'fromEither' function takes a default value and an 'Either'
--   value.  If the 'Either' is 'Left', it returns the default value;
--   otherwise, it returns the value contained in the 'Right'.
fromEither     :: a -> Either e a -> a
fromEither d x = case x of {Left _ -> d; Right v  -> v}

-- | Takes a list of 'Either's and returns a list of all the 'Right'
--   values.
catEithers              :: [Either e a] -> [a]
catEithers ls = [x | Right x <- ls]

-- | Like modify, but the function that maps the old state to the
--   new state operates in the inner monad.
--   For example,
--
--   > s <- get
--   > s' = lift $ f s
--   > put s'
--
--   can be replaced with
--
--   > modifyLift f
modifyLift :: Monad m => (s -> m s) -> StateT s m ()
modifyLift f = get >>= lift . f >>= put

-- | Invoke a function in the inner monad, and pass the state as
--   a parameter.
--   Similar to modifyLift, but the function being invoked doesn't
--   have a return value, so the state is not modified.
--   For example,
--
--   > s <- get
--   > s' = lift $ f s
--
--   can be replaced with
--
--   > getLift f
getLift :: Monad m => (s -> m ()) -> StateT s m ()
getLift f = get >>= lift . f >> return ()

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

-- | Convert a list of bits to a string of @0@s and @1@s.
boolsToBits :: [Bool] -> String
boolsToBits = map (\b -> if b then '1' else '0')

-- | Show /non-negative/ 'Integral' numbers in binary.
showBin :: (Integral a,Show a) => a -> ShowS
showBin = showIntAtBase 2 intToDigit
