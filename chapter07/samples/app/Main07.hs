{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main07 where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as B

-- 7.6.1

data Human = Human
  { name :: String,
    age :: Int
  }
  deriving (Show)

deriveJSON defaultOptions ''Human

taro :: Human
taro = Human {name = "Taro", age = 30}

hanako :: B.ByteString
hanako = "{\"name\":\"Hanako\",\"age\":25}"

jiro :: B.ByteString
jiro = "{\"onamae\":\"Jiro\",\"nenrei\":30}"

main :: IO ()
main = do
  B.putStrLn . encode $ taro

  print (decode hanako :: Maybe Human)
  print (decode jiro :: Maybe Human)

  print (eitherDecode hanako :: Either String Human)
  print (eitherDecode jiro :: Either String Human)
