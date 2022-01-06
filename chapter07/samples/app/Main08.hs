{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main08 where

import Data.Aeson
  ( KeyValue ((.=)),
    Value (Array, Number, String),
    encode,
    object,
  )
import qualified Data.ByteString.Lazy.Char8 as B

-- 7.6.2

nameListValue :: Value
nameListValue =
  Array
    [ object
        [ "coworkers"
            .= Array
              [ object
                  ["age" .= Number 20, "name" .= String "Satoshi"],
                object
                  ["age" .= Number 23, "name" .= String "Takeshi"]
              ],
          "departmentName" .= String "Planning"
        ]
    ]

main :: IO ()
main = do
  B.putStrLn (encode nameListValue)
