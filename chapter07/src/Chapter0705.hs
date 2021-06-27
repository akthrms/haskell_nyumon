{-# LANGUAGE OverloadedStrings #-}

module Chapter0705 where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.Text
  ( Parser,
    char,
    count,
    decimal,
    digit,
    double,
    string,
    (<?>),
  )
import Data.List.Split (splitOn)
import qualified Data.Text as T

-- 7.5.1

data YMD = YMD Int Int Int deriving (Show)

data HMS = HMS Int Int Int deriving (Show)

parseYMD :: String -> Maybe YMD
parseYMD =
  listToYMD . splitOn "/"
  where
    listToYMD (y : m : d : _) = Just $ YMD (read y) (read m) (read d)
    listToYMD _ = Nothing

parseHMS :: String -> Maybe HMS
parseHMS =
  listToYMD . splitOn ":"
  where
    listToYMD (h : m : s : _) = Just $ HMS (read h) (read m) (read s)
    listToYMD _ = Nothing

parseDateTime :: String -> Maybe (YMD, HMS)
parseDateTime =
  listToDateTime . splitOn " "
  where
    listToDateTime (ymd : hms : _) = (,) <$> parseYMD ymd <*> parseHMS hms
    listToDateTime _ = Nothing

-- 7.5.2

-- >>> parse decimal "1000"
-- Partial _
-- >>> parse decimal "1000" `feed` ""
-- Done "" 1000

twoOfDecimal :: Parser (Int, Int)
twoOfDecimal = do
  left <- decimal
  char ','
  right <- decimal
  pure (left, right)

parentheses :: Parser a -> Parser a
parentheses parser = do
  char '('
  result <- parser
  char ')'
  pure result

-- >>> parse (parentheses twoOfDecimal) "(123,456)" `feed` ""
-- Done "" (123,456)

data Animal = Dog | Pig deriving (Show)

animal :: Parser Animal
animal = (string "Dog" >> pure Dog) <|> (string "Pig" >> pure Pig)

-- >>> parse animal "Dog" `feed` ""
-- Done "" Dog
-- >>> parse animal "Pig" `feed` ""
-- Done "" Pig
-- >>> parse animal "Cat" `feed` ""
-- Fail "Cat" [] "string"

-- 7.5.3

countRead :: Read a => Int -> Parser Char -> Parser a
countRead i = fmap read . count i

ymdParser :: Parser YMD
ymdParser =
  YMD
    <$> countRead 4 digit <* char '/'
    <*> countRead 2 digit <* char '/'
    <*> countRead 2 digit

hmsParser :: Parser HMS
hmsParser =
  HMS
    <$> countRead 2 digit <* char ':'
    <*> countRead 2 digit <* char ':'
    <*> countRead 2 digit

dateTimeParser :: Parser (YMD, HMS)
dateTimeParser =
  (,)
    <$> ymdParser
    <* char ' '
    <*> hmsParser

-- 7.5.4

-- >>> parse dateTimeParser "2018/08/21hoge 12:00:00" `feed` ""
-- Fail "hoge 12:00:00" ["' '"] "Failed reading: satisfy"
-- >>> parse dateTimeParser "2018/08/21 12:00:00hoge" `feed` ""
-- Done "hoge" (YMD 2018 8 21,HMS 12 0 0)

-- >>> parse (dateTimeParser <* endOfInput) "2018/08/21 12:00:00hoge" `feed` ""
-- Fail "hoge" [] "endOfInput"

ymdParser' :: Parser YMD
ymdParser' =
  YMD
    <$> (countRead 4 digit <?> "Year")
    <* (char '/' <?> "Delimiter Y/M")
    <*> (countRead 2 digit <?> "Month")
    <* (char '/' <?> "Delimiter M/D")
    <*> (countRead 2 digit <?> "Day")

hmsParser' :: Parser HMS
hmsParser' =
  HMS
    <$> (countRead 2 digit <?> "Hour")
    <* (char ':' <?> "Delimiter H:M")
    <*> (countRead 2 digit <?> "Minute")
    <* (char ':' <?> "Delimiter M:S")
    <*> (countRead 2 digit <?> "Second")

dateTimeParser' :: Parser (YMD, HMS)
dateTimeParser' =
  (,)
    <$> (ymdParser' <?> "YMD")
    <* (char ' ' <?> "space")
    <*> (hmsParser' <?> "HMS")

-- >>> parse (dateTimeParser' <* endOfInput) "2018/08/21hoge 12:00:00" `feed` ""
-- Fail "hoge 12:00:00" ["space","' '"] "Failed reading: satisfy"
-- >>> parse (dateTimeParser' <* endOfInput) "2018/08/21 12:00.00" `feed` ""
-- Fail ".00" ["HMS","Delimiter M:S","':'"] "Failed reading: satisfy"

-- 7.5.5

newtype Term = Add Expr deriving (Show)

data Expr = ExprTerm Double Term | ExprEnd Double deriving (Show)

termParser :: Parser Term
termParser =
  addParser
  where
    addParser = Add <$ char '+' <*> exprParser

exprParser :: Parser Expr
exprParser = ExprTerm <$> double <*> termParser <|> ExprEnd <$> double

-- >>> parse (exprParser <* endOfInput) "1+2+3" `feed` ""
-- Done "" (ExprTerm 1.0 (Add (ExprTerm 2.0 (Add (ExprEnd 3.0)))))
