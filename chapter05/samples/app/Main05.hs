module Main05 where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)

-- 5.3.2

data Gender = Man | Woman deriving (Show)

data Human = Human
  { name :: String,
    age :: Int,
    gender :: Gender
  }
  deriving (Show)

-- 5.3.3

assoc :: [(String, Int)]
assoc = [("hiratara", 39), ("shu1", 0), ("masaharu", 32)]

main :: IO ()
main = do
  print (Human "Taro" 10 Man)
  print (Human <$> Just "Taro" <*> pure 10 <*> pure Man)
  print (Human <$> Just "Taro" <*> Nothing <*> pure Man)

  print (lookup "hiratara" assoc)
  print (lookup "homma" assoc)
  print (lookup "homma" assoc <|> lookup "hiratara" assoc)

  print $ do
    age <- lookup "homma" assoc <|> lookup "hiratara" assoc
    guard (age < 20)
    pure age
