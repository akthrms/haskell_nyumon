module Main02 where

import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin)

-- 4.3.1

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering

  x <- getChar
  print x

  x <- getChar
  print x

  x <- getChar
  print x

  xs <- getLine
  putStrLn xs
