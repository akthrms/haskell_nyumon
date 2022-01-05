{-# LANGUAGE OverloadedStrings #-}

module Main04 where

-- 7.5.2

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.Text
  ( Parser,
    char,
    decimal,
    feed,
    parse,
    string,
  )

twoOfDecimal :: Parser (Int, Int)
twoOfDecimal = do
  left <- decimal
  char ','
  right <- decimal
  pure (left, right)

parens :: Parser a -> Parser a
parens parser = do
  char '('
  res <- parser
  char ')'
  pure res

data Animal = Dog | Pig deriving (Show)

animal :: Parser Animal
animal = (string "Dog" >> pure Dog) <|> (string "Pig" >> pure Pig)

main :: IO ()
main = do
  print (parse decimal "1000" `feed` "")

  print (parse (parens twoOfDecimal) "(123,456)" `feed` "")

  print (parse animal "Dog" `feed` "")
  print (parse animal "Pig" `feed` "")
  print (parse animal "Cat" `feed` "")
