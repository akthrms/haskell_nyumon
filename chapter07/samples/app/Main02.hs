module Main02 where

-- 7.4

import Control.Monad (forM_)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

main :: IO ()
main = do
  let animals1 = V.fromList ["Dog", "Pig", "Cat", "Fox", "Mouse", "Cow", "Horse"]
  print (animals1 V.! 3)
  print (animals1 V.!? 999)

  print . V.sum . V.map length $ animals1

  animals2 <- VM.new 5
  VM.write animals2 0 "Dog"
  VM.write animals2 1 "Pig"
  VM.write animals2 2 "Cat"
  VM.write animals2 3 "Fox"
  VM.write animals2 4 "Mouse"

  tmp <- VM.read animals2 1
  VM.write animals2 1 =<< VM.read animals2 3
  VM.write animals2 3 tmp

  forM_ [0 .. VM.length animals2 - 1] $ \i -> do
    putStrLn =<< VM.read animals2 i

  let animals3 = V.fromList ["Dog", "Pig", "Cat", "Fox", "Mouse", "Cow", "Horse"]
  animals4 <- V.thaw animals3
  VM.write animals4 3 "Wolf"
  print =<< V.freeze animals4
