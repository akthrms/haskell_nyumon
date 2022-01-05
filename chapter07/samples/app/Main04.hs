{-# LANGUAGE OverloadedStrings #-}

module Main04 where

-- 7.5.2

import Data.Attoparsec.Text (decimal, feed, parse)

main :: IO ()
main = do
  print (parse decimal "1000" `feed` "")
