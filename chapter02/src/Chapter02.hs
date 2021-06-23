module Chapter02 where

import Control.Monad (forM_, join, when)
import System.Environment (getEnv)

-- 2.5

run :: IO ()
run = putStrLn "Hello, world!"

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

putW :: IO ()
putW = putStrLn "W"

putX :: IO ()
putX = putStrLn "X"

makePutY :: IO (IO ())
makePutY = pure $ putStrLn "Y"

makePutZ :: IO (IO ())
makePutZ = pure $ putStrLn "Z"

run4 :: IO ()
-- run4 = do
--   let w = putW
--       x = putX
--   putY <- makePutY
--   putZ <- makePutZ
--   putZ
run4 = do
  let w = putW
      x = putX
  putY <- makePutY
  join makePutZ

-- 2.7.3

percentage :: (Eq a, Fractional a) => a -> a -> Maybe a
percentage k n
  | n == 0 = Nothing
  | otherwise = Just $ 100.0 * k / n

-- >>> maybe "UNKNOWN" show $ percentage 20 50
-- "40.0"

-- 2.7.4

percentage' :: (Eq b, Fractional b) => b -> b -> Either String b
percentage' k n
  | n == 0 = Left "Illegal division by zero"
  | otherwise = Right $ 100.0 * k / n

-- >>> either id show $ percentage' 20 50
-- "40.0"

-- 2.8.1

run5 :: IO ()
run5 = loop 0
  where
    loop n
      | n <= 20 = do
        when (n `mod` 3 /= 0 && n `mod` 5 /= 0) (putStr $ show n)
        when (n `mod` 3 == 0) (putStr "Fizz")
        when (n `mod` 5 == 0) (putStr "Buzz")
        putStrLn ""
        loop $ n + 1
      | otherwise = pure ()

-- 2.8.2

run6 :: IO ()
run6 = foldr (f . fizzbuzz) (pure ()) [1 .. 20]
  where
    fizzbuzz n
      | n `mod` 15 == 0 = "FizzBuzz"
      | n `mod` 3 == 0 = "Fizz"
      | n `mod` 5 == 0 = "Buzz"
      | otherwise = show n
    f str act = do
      putStrLn str
      act

run7 :: IO ()
run7 = do
  forM_ [1 .. 20] $ \i -> do
    putStrLn $ fizzbuzz i
  where
    fizzbuzz n
      | n `mod` 15 == 0 = "FizzBuzz"
      | n `mod` 3 == 0 = "Fizz"
      | n `mod` 5 == 0 = "Buzz"
      | otherwise = show n
