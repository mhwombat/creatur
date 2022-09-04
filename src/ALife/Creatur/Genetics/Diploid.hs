------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Genetics.Diploid
-- Copyright   :  (c) 2013-2022 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- TODO
--
------------------------------------------------------------------------
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module ALife.Creatur.Genetics.Diploid
  (
    Diploid(..),
    expressMaybe,
    -- * Convenience functions for deriving instances of @Diploid@
    ordMin,
    integralMidpoint,
    floatingMidpoint
    -- * Deriving generic instances of @Diploid@
    -- $Generic
  ) where

import           Data.Word
import           GHC.Generics

-- | A /diploid/ agent has two complete sets of genetic instructions.
--   Instances of this class can be thought of as paired genes or
--   paired instructions for building an agent.
--   When two instructions in a pair differ, /dominance/ relationships
--   determine how the genes will be /expressed/ in the agent.
--   Minimal complete definition: @'express'@.
class Diploid g where
  -- | Given two possible forms of a gene, @'express'@ takes into
  --   account any dominance relationship, and returns a gene
  --   representing the result.
  express :: g -> g -> g

  default express :: (Generic g, GDiploid (Rep g)) => g -> g -> g
  express x y = to $ gexpress (from x) (from y)

class GDiploid f where
  gexpress :: f g -> f g -> f g

-- | Unit: used for constructors without arguments
instance GDiploid U1 where
  gexpress U1 U1 = U1

-- | Constants, additional parameters and recursion of kind *
instance (GDiploid a, GDiploid b) => GDiploid (a :*: b) where
  gexpress (a :*: b) (c :*: d) = gexpress a c :*: gexpress b d

-- | Meta-information (constructor names, etc.)
instance (GDiploid a, GDiploid b) => GDiploid (a :+: b) where
  gexpress (L1 x) (L1 y) = L1 (gexpress x y)
  gexpress (L1 x) _      = L1 x
  gexpress _ (L1 x)      = L1 x
  gexpress (R1 x) (R1 y) = R1 (gexpress x y)

-- | Sums: encode choice between constructors
instance (GDiploid a) => GDiploid (M1 i c a) where
  gexpress (M1 x) (M1 y) = M1 (gexpress x y)

-- | Products: encode multiple arguments to constructors
instance (Diploid a) => GDiploid (K1 i a) where
  gexpress (K1 x) (K1 y) = K1 (express x y)

instance Diploid Bool where
  express a b = a || b

instance Diploid Char where
  express = ordMin

instance Diploid Int where
  express = integralMidpoint

instance Diploid Word where
  express = integralMidpoint

instance Diploid Word8 where
  express = integralMidpoint

instance Diploid Word16 where
  express = integralMidpoint

instance Diploid Word32 where
  express = integralMidpoint

instance Diploid Word64 where
  express = integralMidpoint

instance Diploid Double where
  express = floatingMidpoint

instance (Diploid a) => Diploid [a]

instance (Diploid a) => Diploid (Maybe a)

instance (Diploid a, Diploid b) => Diploid (a, b)

{-
    Convenience functions for implementing Diploid
-}

-- | This function may be used to derive instances of @Diploid@ for
--   ordinal types.
--   The result is the minimum of the two inputs.
ordMin :: Ord a => a -> a -> a
ordMin = min

-- | This function may be used to derive instances of @Diploid@ for
--   integral types.
--   The result is the mean of the two inputs, rounded to the nearest
--   integer.
integralMidpoint :: Integral a => a -> a -> a
integralMidpoint x y = round $ (x' + y')/2
  where x' = fromIntegral x :: Double
        y' = fromIntegral y :: Double

-- | This function may be used to derive instances of @Diploid@ for
--   floating point types.
--   The result is the mean of the two inputs.
floatingMidpoint :: (Num a, Fractional a) => a -> a -> a
floatingMidpoint x y = (x + y)/2


{- $Generic
You can easily use the generic mechanism provided to automatically
create implementations of @Diploid@ for arbitrarily complex types.
First, you need to import:

>import GHC.Generics

Instances of @Diploid@ have been defined for some base types.
You will need to create instances for any additional base types
that you use.

If the arrays are of different lengths, the result will be as long as
the shorter array.

>λ> express [1,2,3,4] [5,6,7,8,9] :: [Int]
>[1,2,3,4]

You can automatically derive instances for more complex types:

>data MyType = MyTypeA Bool | MyTypeB Int | MyTypeC Bool Int [MyType]
>deriving (Show, Generic)

>instance Diploid MyType
>instance Diploid [MyType]

>λ> express (MyTypeA True) (MyTypeA False)
>MyTypeA True

>λ> express (MyTypeB 2048) (MyTypeB 36)
>MyTypeB 36

Even with complex values, the implementation should just
"do the right thing".

>λ> express (MyTypeC False 789 [MyTypeA True, MyTypeB 33, MyTypeC True 12 []]) (MyTypeC True 987 [MyTypeA False, MyTypeB 11, MyTypeC True 3 []])
>MyTypeC True 789 [MyTypeA True,MyTypeB 11,MyTypeC True 3 []]

When a type has multiple constructors, the constructors that appear
earlier in the definition are dominant over those that appear later.
For example:

>λ> express (MyTypeA True) (MyTypeB 7)
>MyTypeA True

>λ> express (MyTypeB 4) (MyTypeC True 66 [])
>MyTypeB 4

-}

expressMaybe :: Diploid g => Maybe g -> Maybe g -> Maybe g
expressMaybe (Just a) (Just b) = Just (express a b)
expressMaybe (Just a) Nothing  = Just a
expressMaybe Nothing (Just b)  = Just b
expressMaybe Nothing Nothing   = Nothing
