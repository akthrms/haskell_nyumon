{-# LANGUAGE OverloadedStrings #-}

module Data.Hjq (hjq) where

import Control.Error.Util (note)
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B
import Data.Functor ((<&>))
import Data.Hjq.Parser (parseJqQuery)
import Data.Hjq.Query (executeQuery)
import qualified Data.Text as T (Text)

hjq :: B.ByteString -> T.Text -> Either T.Text B.ByteString
hjq jsonString queryString = do
  value <- note "Invalid json format" (decode jsonString)
  query <- parseJqQuery queryString
  executeQuery query value <&> encodePretty
