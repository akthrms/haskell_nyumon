module Chapter0704 where

import Control.Monad (forM_)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- 7.4.1

animals :: V.Vector String
animals = V.fromList ["Dog", "Pig", "Cat", "Fox", "Mouse", "Cow", "Horse"]

-- >>> V.sum . V.map length $ animals
-- 25

-- 7.4.2

run :: IO ()
run = do
  animals <- VM.new 5
  VM.write animals 0 "Dog"
  VM.write animals 1 "Pig"
  VM.write animals 2 "Cat"
  VM.write animals 3 "Fox"
  VM.write animals 4 "Mouse"

  tmp <- VM.read animals 1
  VM.write animals 1 =<< VM.read animals 3
  VM.write animals 3 tmp

  forM_ [0 .. (VM.length animals - 1)] $ \i -> do
    putStrLn =<< VM.read animals i

-- 7.4.3

run2 :: IO ()
run2 = do
  mAnimals <- V.thaw animals
  VM.write mAnimals 3 "Wolf"
  print =<< V.freeze mAnimals
