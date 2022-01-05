{-# LANGUAGE OverloadedStrings #-}

module Main06 where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.Text
  ( Parser,
    char,
    double,
    endOfInput,
    feed,
    parse,
  )

-- 7.5.5

newtype Term = Add Expr deriving (Show)

data Expr = ExTerm Double Term | ExEnd Double deriving (Show)

termParser :: Parser Term
termParser = Add <$ char '+' <*> exprParser

exprParser :: Parser Expr
exprParser = ExTerm <$> double <*> termParser <|> ExEnd <$> double

main :: IO ()
main = do
  print (parse (exprParser <* endOfInput) "1+2+3" `feed` "")
