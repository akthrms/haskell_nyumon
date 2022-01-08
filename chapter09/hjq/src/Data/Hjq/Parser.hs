{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq.Parser where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.Text
  ( IResult (Done),
    Parser,
    Result,
    char,
    decimal,
    digit,
    endOfInput,
    feed,
    letter,
    many1,
    parse,
    sepBy,
    skipSpace,
  )
import Data.Text (Text, pack)

data JqFilter
  = JqField Text JqFilter
  | JqIndex Int JqFilter
  | JqNil
  deriving (Show, Read, Eq)

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = showParseResult (parse (jqFilterParser <* endOfInput) s `feed` "")

jqFilterParser :: Parser JqFilter
jqFilterParser = schar '.' *> (jqField <|> jqIndex <|> jqNil)
  where
    jqFilter :: Parser JqFilter
    jqFilter = (schar '.' *> jqField) <|> jqIndex <|> jqNil

    jqField :: Parser JqFilter
    jqField = JqField <$> (word <* skipSpace) <*> jqFilter

    jqIndex :: Parser JqFilter
    jqIndex = JqIndex <$> (schar '[' *> decimal <* schar ']') <*> jqFilter

    jqNil :: Parser JqFilter
    jqNil = pure JqNil

showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left (pack (show r))

data JqQuery
  = JqQueryObject [(Text, JqQuery)]
  | JqQueryArray [JqQuery]
  | JqQueryFilter JqFilter
  deriving (Show, Read, Eq)

parseJqQuery :: Text -> Either Text JqQuery
parseJqQuery s = showParseResult (parse (jqQueryParser <* endOfInput) s `feed` "")

jqQueryParser :: Parser JqQuery
jqQueryParser = queryArray <|> queryFilter <|> queryObject
  where
    queryArray :: Parser JqQuery
    queryArray = JqQueryArray <$> (schar '[' *> (jqQueryParser `sepBy` schar ',') <* schar ']')

    queryObject :: Parser JqQuery
    queryObject = JqQueryObject <$> (schar '{' *> (qObj `sepBy` schar ',') <* schar '}')

    qObj :: Parser (Text, JqQuery)
    qObj = (,) <$> (schar '"' *> word <* schar '"') <*> (schar ':' *> jqQueryParser)

    queryFilter :: Parser JqQuery
    queryFilter = JqQueryFilter <$> jqFilterParser

word :: Parser Text
word = fmap pack (many1 (letter <|> char '-' <|> char '_' <|> digit))

schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace
