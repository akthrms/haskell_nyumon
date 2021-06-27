{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter0702 where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics

-- 7.6.1

data Human = Human {name :: String, age :: Int} deriving (Show)

deriveJSON defaultOptions ''Human

taro :: Human
taro = Human {name = "Taro", age = 30}

hanako :: B.ByteString
hanako = "{\"name\":\"Hanako\",\"age\":25}"

jiro :: B.ByteString
jiro = "{\"onamae\":\"Jiro\",\"nenrei\":30}"

-- >>> encode $ taro
-- "{\"name\":\"Taro\",\"age\":30}"
-- >>> decode hanako :: Maybe Human
-- Just (Human {name = "Hanako", age = 25})
-- >>> decode jiro :: Maybe Human
-- Nothing

-- >>> eitherDecode hanako :: Either String Human
-- Right (Human {name = "Hanako", age = 25})
-- >>> eitherDecode jiro :: Either String Human
-- Left "Error in $: When parsing the record Human of type Chapter0702.Human the key name was not present."

-- >>> encode (["Taro", "Jiro", "Hanako"] :: [String])
-- "[\"Taro\",\"Jiro\",\"Hanako\"]"
-- >>> encode ([10, 20, 30] :: [Int])
-- "[10,20,30]"
-- >>> encode (("Hello", 100) :: (String, Int))
-- "[\"Hello\",100]"
-- >>> decode "[\"Taro\", \"Jiro\", \"Hanako\"]" :: Maybe [String]
-- Just ["Taro","Jiro","Hanako"]
-- >>> decode "[10, 20, 30]" :: Maybe [Int]
-- Just [10,20,30]
-- >>> decode "[777 , \"Haskell\"]" :: Maybe (Int, String)
-- Just (777,"Haskell")

data Department = Department {departmentName :: String, coworkers :: [Human]} deriving (Show)

deriveJSON defaultOptions ''Department

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

-- >>> encode (nameList !! 0)
-- "{\"departmentName\":\"General Affairs\",\"coworkers\":[{\"name\":\"Taro\",\"age\":30},{\"name\":\"Matsuko\",\"age\":26}]}"

data IntString = IntData Int | StringData String

deriveJSON defaultOptions ''IntString

-- >>> encode $ IntData 999
-- "{\"tag\":\"IntData\",\"contents\":999}"
-- >>> encode $ StringData "World!"
-- "{\"tag\":\"StringData\",\"contents\":\"World!\"}"

-- 7.6.2

nameListValue :: Value
nameListValue =
  Array
    [ object
        [ "coworkers"
            .= Array
              [ object
                  [ "age" .= Number 20,
                    "name" .= String "Satoshi"
                  ],
                object
                  [ "age" .= Number 23,
                    "name" .= String "Takeshi"
                  ]
              ],
          "departmentName" .= String "String Planning"
        ]
    ]

-- >>> encode nameListValue
-- "[{\"coworkers\":[{\"name\":\"Satoshi\",\"age\":20},{\"name\":\"Takeshi\",\"age\":23}],\"departmentName\":\"String Planning\"}]"

data Department' = Department' {departmentName' :: String, coworkers' :: [Human]} deriving (Show)

deriveJSON
  ( defaultOptions
      { fieldLabelModifier = \label -> case label of
          "departmentName'" -> "name"
          _ -> label
      }
  )
  ''Department'

data Person = Person {personName :: String, personAge :: Int} deriving (Show)

instance ToJSON Person where
  toJSON (Person n a) = object ["name" .= n, "age" .= a]

instance FromJSON Person where
  parseJSON (Object v) = Person <$> v .: "name" <*> v .: "age"
  parseJSON v = typeMismatch "Person" v

data Person' = Person' {personName' :: String, personAge' :: Int} deriving (Show, Generic)

instance ToJSON Person'

instance FromJSON Person'
