{-# LANGUAGE OverloadedStrings #-}

module Chapter0701 where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text hiding (take)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- 7.3.1

cat :: BS.ByteString
cat = "Meow!"

dog :: BS.ByteString
dog = "Bowwow!"

-- 7.4.1

animals :: V.Vector String
animals = V.fromList ["Dog", "Pig", "Cat", "Fox", "Mouse", "Cow", "Horse"]

-- >>> V.sum . V.map length $ animals
-- 25

-- 7.4.2

run :: IO ()
run = do
  animals <- VM.new 5
  VM.write animals 0 "Dog"
  VM.write animals 1 "Pig"
  VM.write animals 2 "Cat"
  VM.write animals 3 "Fox"
  VM.write animals 4 "Mouse"
  VM.write animals 5 "Cow"
  VM.write animals 6 "Horse"

  tmp <- VM.read animals 1
  VM.write animals 1 =<< VM.read animals 3
  VM.write animals 3 tmp

  forM_ [0 .. VM.length animals - 1] $ \i -> do
    putStrLn =<< VM.read animals i

-- 7.4.3

run2 :: IO ()
run2 = do
  animals' <- V.thaw animals
  VM.write animals' 3 "Wolf"
  print =<< V.freeze animals'

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
  listToHMS . splitOn ":"
  where
    listToHMS (h : m : s : _) = Just $ HMS (read h) (read m) (read s)
    listToHMS _ = Nothing

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

animalParser :: Parser Animal
animalParser = (string "Dog" >> pure Dog) <|> (string "Pig" >> return Pig)

-- >>> parse animalParser "Dog" `feed` ""
-- Done "" Dog
-- >>> parse animalParser "Pig" `feed` ""
-- Done "" Pig
-- >>> parse animalParser "Cat" `feed` ""
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

dataTimeParser :: Parser (YMD, HMS)
dataTimeParser = (,) <$> ymdParser <* char ' ' <*> hmsParser

-- 7.5.4

-- >>> parse dataTimeParser "2018/08/21hoge 12:00:00"
-- Fail "hoge 12:00:00" ["' '"] "Failed reading: satisfy"
-- >>> parse dataTimeParser "2018/08/21 12:00:00hoge"
-- Done "hoge" (YMD 2018 8 21,HMS 12 0 0)
-- >>> parse (dataTimeParser <* endOfInput) "2018/08/21 12:00:00hoge"
-- Fail "hoge" [] "endOfInput"

ymdParser' :: Parser YMD
ymdParser' =
  YMD
    <$> countRead 4 digit <* (char '/' <?> "Delimiter Y/M")
    <*> countRead 2 digit <* (char '/' <?> "Delimiter M/D")
    <*> countRead 2 digit

hmsParser' :: Parser HMS
hmsParser' =
  HMS
    <$> countRead 2 digit <* (char ':' <?> "Delimiter H:M")
    <*> countRead 2 digit <* (char ':' <?> "Delimiter M:S")
    <*> countRead 2 digit

dataTimeParser' :: Parser (YMD, HMS)
dataTimeParser' =
  (,)
    <$> (ymdParser <?> "YMD")
    <* (char ' ' <?> "space")
    <*> (hmsParser <?> "HMS")

-- >>> parse (dataTimeParser' <* endOfInput) "2018/08/21hoge 12:00:00"
-- Fail "hoge 12:00:00" ["space","' '"] "Failed reading: satisfy"
-- >>> parse (dataTimeParser' <* endOfInput) "2018/08/21 12:00.00"
-- Fail ".00" ["HMS","':'"] "Failed reading: satisfy"

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
