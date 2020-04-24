{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import Data.Word

class Genetic s g where
  -- | Writes a gene to a sequence.
  put :: s -> g -> s
  -- | Reads the next gene in a sequence.
  get :: s -> (g, s)

data SampleGene = Variant1 | Variant2 deriving Show

instance Genetic [Bool] SampleGene where
  put xs Variant1 = True  : xs
  put xs Variant2 = False : xs
  get (True  : xs) = (Variant1, xs)
  get (False : xs) = (Variant2, xs)
  get _ = error "coding error"

instance Genetic [Word8] SampleGene where
  put xs Variant1 = 0 : xs
  put xs Variant2 = 1 : xs
  get (0 : xs) = (Variant1, xs)
  get (1 : xs) = (Variant2, xs)
  get _ = error "coding error"

main :: IO ()
main = do
  putStrLn "Working with Bool sequences"
  let xs = put [] Variant1 :: [Bool]
  let (g,_) = get xs :: (SampleGene, [Bool])
  putStrLn $ "Read " ++ show g
  putStrLn "Working with Bool sequences"
  let xs2 = put [] Variant1 :: [Word8]
  let (g2,_) = get xs2 :: (SampleGene, [Word8])
  putStrLn $ "Read " ++ show g2
