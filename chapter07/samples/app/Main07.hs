{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main07 where

import Data.Aeson (decode, defaultOptions, eitherDecode, encode)
import Data.Aeson.TH
  ( Options (fieldLabelModifier),
    defaultOptions,
    deriveJSON,
  )
import qualified Data.ByteString.Lazy.Char8 as B

-- 7.6.1

data Human = Human
  { name :: String,
    age :: Int
  }
  deriving (Show)

data Department = Department
  { departmentName :: String,
    coworkers :: [Human]
  }
  deriving (Show)

deriveJSON defaultOptions ''Human

-- 7.6.3

deriveJSON
  ( defaultOptions
      { fieldLabelModifier = \s -> case s of
          "departmentName" -> "name"
          t -> t
      }
  )
  ''Department

taro :: Human
taro = Human {name = "Taro", age = 30}

hanako :: B.ByteString
hanako = "{\"name\":\"Hanako\",\"age\":25}"

jiro :: B.ByteString
jiro = "{\"onamae\":\"Jiro\",\"nenrei\":30}"

saburo :: Human
saburo = Human {name = "Saburo", age = 31}

shiro :: Human
shiro = Human {name = "Shiro", age = 31}

matsuko :: Human
matsuko = Human {name = "Matsuko", age = 26}

nameList :: [Department]
nameList =
  [ Department {departmentName = "General Affairs", coworkers = [taro, matsuko]},
    Department {departmentName = "Development", coworkers = [saburo, shiro]}
  ]

data IntStr = IntData Int | StrData String

deriveJSON defaultOptions ''IntStr

main :: IO ()
main = do
  B.putStrLn (encode taro)

  print (decode hanako :: Maybe Human)
  print (decode jiro :: Maybe Human)

  print (eitherDecode hanako :: Either String Human)
  print (eitherDecode jiro :: Either String Human)

  B.putStrLn (encode nameList)

  B.putStrLn (encode (IntData 999))
  B.putStrLn (encode (StrData "World!"))
