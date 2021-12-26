module Main06 where

import Control.Monad (forM_, when)

-- 2.8.1

main :: IO ()
main = loop 0
  where
    loop :: Int -> IO ()
    loop n
      | n <= 20 = do
        when (n `mod` 3 /= 0 && n `mod` 5 /= 0) (print n)
        when (n `mod` 3 == 0) (putStr "Fizz")
        when (n `mod` 5 == 0) (putStr "Buzz")
        putStrLn ""
        loop (n + 1)
      | otherwise = pure ()

-- 2.8.2

main' :: IO ()
main' =
  --   foldr f (pure ()) $ map fizzbuzz [1 .. 20]
  foldr (f . fizzbuzz) (pure ()) [1 .. 20]
  where
    fizzbuzz :: Int -> String
    fizzbuzz n
      | n `mod` 15 == 0 = "FizzBuzz"
      | n `mod` 3 == 0 = "Fizz"
      | n `mod` 5 == 0 = "Buzz"
      | otherwise = show n
    f :: String -> IO () -> IO ()
    f str act = do
      putStrLn str
      act

main'' :: IO ()
main'' = do
  forM_ [1 .. 20] $ \i -> do
    putStrLn $ fizzbuzz i
  where
    fizzbuzz :: Int -> String
    fizzbuzz n
      | n `mod` 15 == 0 = "FizzBuzz"
      | n `mod` 3 == 0 = "Fizz"
      | n `mod` 5 == 0 = "Buzz"
      | otherwise = show n
