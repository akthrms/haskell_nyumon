module Chapter02 where

import Control.Monad (forM_, when)
-- import Data.Either
-- import Data.Maybe
import System.Environment (getEnv)

run :: IO ()
run = putStrLn "Hello, world!"

-- run2 =
--   putStrLn "Hello, world!"
--   putStrLn "from first hs file."

run2 :: IO ()
run2 = do
  putStrLn "Hello, world!"
  putStrLn "from first hs file."

run3 :: IO ()
run3 = do
  let title = "Current User:"
  user <- getEnv "USER"
  putStrLn title
  putStrLn user

percentage :: (Eq a, Fractional a) => a -> a -> Maybe a
percentage k n
  | n == 0 = Nothing
  | otherwise = Just $ 100.0 * k / n

run4 :: IO ()
run4 = do
  let p = percentage 20 50
  --   putStrLn $ if isNothing p then "UNKNOWN" else show $ fromJust p
  putStrLn $ maybe "UNKNOWN" show p

percentage2 :: (Eq b, Fractional b) => b -> b -> Either String b
percentage2 k n
  | n == 0 = Left "Illegal division by zero"
  | otherwise = Right $ 100.0 * k / n

run5 :: IO ()
run5 = do
  let p = percentage2 20 50
  putStrLn $ either id show p

run6 :: IO ()
run6 = loop 0
  where
    loop n
      | n <= 20 = do
        when (n `mod` 3 /= 0 && n `mod` 5 /= 0) (putStr $ show n)
        when (n `mod` 3 == 0) (putStr "Fizz")
        when (n `mod` 5 == 0) (putStr "Buzz")
        putStrLn ""
        loop $ n + 1
      | otherwise = pure ()

run7 :: IO ()
run7 = foldr (f . fizzbuzz) (pure ()) [1 .. 20]
  where
    fizzbuzz n
      | n `mod` 15 == 0 = "FizzBuzz"
      | n `mod` 3 == 0 = "Fizz"
      | n `mod` 5 == 0 = "Buzz"
      | otherwise = show n
    f str act = do
      putStrLn str
      act

run8 :: IO ()
run8 = do
  forM_ [1 .. 20] $ \i -> do
    putStrLn $ fizzbuzz i
  where
    fizzbuzz n
      | n `mod` 15 == 0 = "FizzBuzz"
      | n `mod` 3 == 0 = "Fizz"
      | n `mod` 5 == 0 = "Buzz"
      | otherwise = show n
