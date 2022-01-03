{-# LANGUAGE OverloadedStrings #-}

module Main01 where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BSC (putStrLn)

-- 7.3.1

cat :: ByteString
cat = "Meow!"

dog :: ByteString
dog = "Bowwow!"

main :: IO ()
main = do
  BSC.putStrLn cat
  BSC.putStrLn dog
