module Main01 where

import Control.Monad.Par (get, runPar, spawnP)
import System.Environment (getArgs)

-- 8.1.2

main :: IO ()
main = do
  args <- getArgs
  let (n : _) = fmap read args
  print (parallelFib n)
  putStrLn "DONE"

parallelFib :: Int -> Int
parallelFib 0 = 0
parallelFib 1 = 1
parallelFib n = runPar $ do
  ivar1 <- spawnP (fib (n - 1))
  ivar2 <- spawnP (fib (n - 2))
  result1 <- get ivar1
  result2 <- get ivar2
  pure (result1 + result2)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
