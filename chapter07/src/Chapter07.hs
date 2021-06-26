{-# LANGUAGE OverloadedStrings #-}

module Chapter07 where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List.Split (splitOn)
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
