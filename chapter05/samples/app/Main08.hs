module Main08 where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST
  ( STUArray,
    getElems,
    newListArray,
    readArray,
    writeArray,
  )
import Data.STRef (modifySTRef, newSTRef, readSTRef)

-- 5.6.1

procCount :: Integer
procCount = runST $ do
  n <- newSTRef 0

  forM_ [1 .. 10] $ \i -> do
    modifySTRef n (+ 1)

  readSTRef n

-- 5.6.2

doubleArray :: [Double]
doubleArray = runST $ do
  arr <- newListArray (0, 4) [1 .. 5] :: ST s (STUArray s Int Double)
  x <- readArray arr 2
  writeArray arr 2 (x * 10.0)
  getElems arr

main :: IO ()
main = do
  print procCount
  print doubleArray
