{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq.Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text

data JqFilter
  = JqField Text JqFilter
  | JqIndex Int JqFilter
  | JqNil
  deriving (Show, Read, Eq)

parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = showParseResult (parse (jqFilterParser <* endOfInput) s `feed` "")

jqFilterParser :: Parser JqFilter
jqFilterParser = skipSpaceChar '.' *> (jqField <|> jqIndex <|> jqNil)
  where
    jqFilter :: Parser JqFilter
    jqFilter = (skipSpaceChar '.' *> jqField) <|> jqIndex <|> jqNil

    jqField :: Parser JqFilter
    jqField = JqField <$> word <* skipSpace <*> jqFilter

    jqIndex :: Parser JqFilter
    jqIndex = JqIndex <$> (skipSpaceChar '[' *> decimal <* skipSpaceChar ']') <*> jqFilter

    jqNil :: Parser JqFilter
    jqNil = pure JqNil

showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left (pack . show $ r)

word :: Parser Text
word = fmap pack (many1 (letter <|> char '-' <|> char '_' <|> digit))

skipSpaceChar :: Char -> Parser Char
skipSpaceChar c = skipSpace *> char c <* skipSpace
