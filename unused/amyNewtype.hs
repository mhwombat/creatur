{-# LANGUAGE TypeFamilies #-}
import Data.Word

class Genetic g where
  type Sequence g :: *
  -- | Writes a gene to a sequence.
  put :: Sequence g -> g -> Sequence g
  -- | Reads the next gene in a sequence.
  get :: Sequence g -> (g, Sequence g)

data SampleGene = Variant1 | Variant2 deriving Show

newtype SampleGene1 = MkSampleGene1 SampleGene

newtype SampleGene2 = MkSampleGene2 SampleGene

instance Genetic SampleGene1 where
  type Sequence SampleGene1 = [Bool]
  put xs (MkSampleGene1 Variant1) = True : xs
  put xs (MkSampleGene1 Variant2) = False : xs
  get (True:xs) = (MkSampleGene1 Variant1, xs)
  get (False:xs) = (MkSampleGene1 Variant2, xs)
  get _ = error "coding error"

instance Genetic SampleGene2 where
  type Sequence SampleGene2 = [Word8]
  put xs (MkSampleGene2 Variant1) = 0 : xs
  put xs (MkSampleGene2 Variant2) = 1 : xs
  get (0:xs) = (MkSampleGene2 Variant1, xs)
  get (1:xs) = (MkSampleGene2 Variant2, xs)
  get _ = error "coding error"
  
-- main = do
--   putStrLn "Working with Bool sequences"
--   let xs = put [] Variant1 :: [Bool]
--   let (g,ys) = get xs :: (SampleGene1, [Bool])
--   putStrLn $ "Read " ++ show g
--   putStrLn "Working with Bool sequences"
--   let xs = put [] Variant1 :: [Word8]
--   let (g,ys) = get xs :: (SampleGene1, [Word8])
--   putStrLn $ "Read " ++ show g
