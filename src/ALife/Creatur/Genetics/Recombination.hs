------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.Recombination
-- Copyright   :  (c) 2011-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides a mechanism to break apart and rejoin sequences of data. 
-- Inspired by DNA recombination in biology, this technique can be used
-- to recombine \"genetic\" instructions for building artificial life.
--
------------------------------------------------------------------------
module ALife.Creatur.Genetics.Recombination
  (
    crossover,
    cutAndSplice,
    mutateList,
    mutatePairedLists,
    randomOneOfList,
    randomOneOfPair,
    randomCrossover,
    randomCutAndSplice,
    repeatWithProbability,
    withProbability
  ) where

import ALife.Creatur.Util (safeReplaceElement)

import System.Random (Random)
import Control.Exception.Base (assert)
import Control.Monad.Random (Rand, RandomGen, getRandom, getRandomR)

-- | Cuts two lists at the specified locations, swaps the ends, and 
--   splices them. The resulting lists will be:
--   @
--     a[0..n-1] ++ b[m..]
--     b[0..m-1] ++ a[n..]
--   @
--   Here are some examples.
--   @
--     /Expression/                               /Result/
--     'cutAndSplice' 2 5 (\"abcdef\", \"ABCDEF\")    (\"abF\",\"ABCDEcdef\")
--     'cutAndSplice' 3 1 (\"abcd\", \"ABCDEFG\")     (\"abcBCDEFG\",\"Ad\")
--     'cutAndSplice' 4 4 (\"abcdef\", \"ABCDEF\")    (\"abcdEF\",\"ABCDef\")
--   @
--   If n <= 0 or m <= 0, the corresponding input list will be completely
--   transferred to the other.
--   @
--     /Expression/                               /Result/
--     'cutAndSplice' 0 4 (\"abcdef\", \"ABCDEF\")    (\"EF\",\"ABCDabcdef\")
--     'cutAndSplice' (-2) 4 (\"abcd\", \"ABCDEFGH\") (\"EFGH\",\"ABCDabcd\")
--     'cutAndSplice' 5 0 (\"abcdef\", \"ABCDEF\")    (\"abcdeABCDEF\",\"f\")
--   @
--   If n or m are greater than or equal to length of the corresponding list,
--   that list will not be transferred.
--   @
--     /Expression/                               /Result/
--     'cutAndSplice' 10 0 (\"abcdef\", \"ABCDEF\")   (\"abcdefABCDEF\",\"\")
--     'cutAndSplice' 0 0 (\"\", \"ABCDEF\")          (\"ABCDEF\",\"\")
--   @
cutAndSplice :: Int -> Int -> ([a], [a]) -> ([a], [a])
cutAndSplice n m (as, bs) = (cs, ds)
    where cs = as1 ++ bs2
          ds = bs1 ++ as2
          (as1, as2) = splitAt n as
          (bs1, bs2) = splitAt m bs

-- | Same as @'cutAndSplice'@, except that the two locations are
--   chosen at random.
randomCutAndSplice :: RandomGen g => ([a], [a]) -> Rand g ([a], [a])
randomCutAndSplice (as, bs) = do
    n <- getRandomR (0,length as - 1)
    m <- getRandomR (0,length bs - 1)
    return (cutAndSplice n m (as, bs))

-- | Cuts two lists at the specified location, swaps the ends, and 
--   splices them. This is a variation of 'cutAndSplice' where n == m.
crossover :: Int -> ([a], [a]) -> ([a], [a])
crossover n = cutAndSplice n n

-- | Same as @'crossover'@, except that the location is chosen at 
--   random.
randomCrossover :: RandomGen g => ([a], [a]) -> Rand g ([a], [a])
randomCrossover (as, bs) = do
    n <- getRandomR (0,length as - 1)
    return (crossover n (as, bs))

-- | Mutates a random element in the list.
mutateList :: (Random n, RandomGen g) => [n] -> Rand g [n]
mutateList xs = do
  (i, _) <- randomListSelection xs
  x <- getRandom
  return (safeReplaceElement xs i x)

-- | Mutates a random element in one list in a pair.
mutatePairedLists :: 
  (Random n, RandomGen g) => ([n], [n]) -> Rand g ([n], [n])
mutatePairedLists (xs,ys) = do
  chooseFst <- weightedRandomBoolean 0.5
  if chooseFst 
    then do
      xs' <- mutateList xs
      return (xs', ys)
    else do
      ys' <- mutateList ys
      return (xs, ys')

-- | Performs an operation with the specified probability.
withProbability :: RandomGen g => Double -> (b -> Rand g b) -> b -> Rand g b
withProbability p op genes = do
  doOp <- weightedRandomBoolean p
  if doOp then op genes else return genes

-- | Performs an operation a random number of times.
--   The probability of repeating the operation @n@ times is @p^n@.
repeatWithProbability :: RandomGen g => Double -> (b -> Rand g b) -> b -> Rand g b
repeatWithProbability p op genes = do
  doOp <- weightedRandomBoolean p
  if doOp 
    then do
      genes' <- op genes
      repeatWithProbability p op genes'
    else return genes


-- :m + ALife.Creatur.Genetics.Gene
-- let g = (replicate 10 A, replicate 10 C)
-- evalRandIO (withProbability 0.1 randomCrossover g >>= withProbability 0.01 randomCutAndSplice >>= withProbability 0.001 mutatePairedLists)
-- Any mixing of As and Cs will be the result of crossover (if the lengths are the same) or cut-and-splice (if the lengths are different).
-- Any Gs or Ts that show up are the result of mutation.
-- evalRandIO (withProbability 0.5 randomCrossover g >>= withProbability 0.05 randomCutAndSplice >>= withProbability 0.5 mutatePairedLists >>= randomOneOfPair)


-- | Randomly select a boolean, but weighted to return True with probability 
--   p.
weightedRandomBoolean :: RandomGen g => Double -> Rand g Bool
weightedRandomBoolean p = do
  x <- getRandomR (0.0,1.0)
  return (x < p)

randomOneOfPair :: RandomGen g => (a, a) -> Rand g a
randomOneOfPair pair = do
  chooseFst <- weightedRandomBoolean 0.5
  if chooseFst 
    then return $ fst pair
    else return $ snd pair

randomOneOfList :: RandomGen g => [a] -> Rand g a
randomOneOfList xs = do
  (_, z) <- randomListSelection xs
  return z

---- | Sample a random element from a weighted list.
----   The total weight of all elements must not be 0.
---- Adapted from the code in MonadRandom
--randomWeightedChoice :: RandomGen g => [(a, Double)] -> Rand g a
--randomWeightedChoice [] = error "randomFromList called with empty list"
--randomWeightedChoice [(x,_)] = return x
--randomWeightedChoice xs = do
--  let s = sum $ map snd xs -- total weight
--  let cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs     -- cumulative weight
--  p <- getRandomR (0.0,s)
--  return (fst (head (dropWhile (\(_,q) -> q < p) cs)))

-- | Choose an element at random from a list and return the element and its 
--   index
randomListSelection :: RandomGen g => [a] -> Rand g (Int, a)
randomListSelection xs = assert (not . null $ xs) $ do
  i <- getRandomR (0,length xs - 1)
  return (i, xs !! i)

