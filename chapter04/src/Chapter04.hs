module Chapter04 where

import Data.Char (toUpper)
import System.Environment (getArgs)

run :: IO ()
run =
  getLine >>= \x ->
    getLine >>= \y ->
      putStrLn ("1つ目の入力: " ++ x) >> putStrLn ("2つ目の入力: " ++ y)

run2 :: IO ()
run2 = do
  x <- getLine
  y <- getLine
  putStrLn $ "1つ目の入力: " ++ x
  putStrLn $ "2つ目の入力: " ++ y

run3 :: IO ()
run3 = do
  x <- getLine
  putStrLn $ "1つ目の入力: " ++ x
  getLine >>= pure . ("2つ目の入力: " ++) >>= putStrLn

run4 :: IO ()
run4 = do
  args <- getArgs
  print args

run5 :: IO ()
run5 = do
  xs <- getContents >>= pure . lines
  counter 0 xs

counter :: Int -> [String] -> IO ()
counter _ [] = pure ()
counter i ("up" : xs) = print (i + 1) >> counter (i + 1) xs
counter i ("down" : xs) = print (i - 1) >> counter (i - 1) xs
counter i (_ : xs) = counter i xs

interact' :: (String -> String) -> IO ()
interact' f = do
  s <- getContents
  putStr $ f s

run6 :: IO ()
run6 = interact' $ map toUpper

run7 :: IO ()
run7 = readFile "sample.txt" >>= putStrLn

run8 :: IO ()
run8 = readFile "sample.txt" >>= putStrLn . reverse

run9 :: IO ()
run9 = do
  val <- readFile "sample.txt"
  putStrLn $ take 5 val
  writeFile "sample.txt" "Hello, Lazy IO!"
