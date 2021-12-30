module Main09 where

import Control.Monad (guard)

-- 5.7

points :: [(Integer, Integer)]
points = do
  x <- [1 .. 3]
  y <- [1 .. 3]
  pure (x, y)

-- 5.7.1

orderedPoints :: [(Integer, Integer)]
orderedPoints = do
  x <- [1 .. 3]
  y <- [1 .. 3]
  guard (x < y)
  pure (x, y)

main :: IO ()
main = do
  print points
  print orderedPoints
  print [(x, y) | x <- [1 .. 3], y <- [1 .. 3], x < y]
