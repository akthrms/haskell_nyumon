{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Chapter06 where

import Control.Monad
import Control.Monad.Reader
import Data.Char
import System.Environment
import System.IO

-- 6.1.2

data Employee = Employee {name :: String, age :: Int, role :: Role}

data Role = Engineers | Sales | Designers

data Tree a = Leaf a | Node a (Tree a) (Tree a)

data Loop a
  = End
  | Step a (Loop a)
  deriving (Show)

data FizzBuzzAction
  = PrintFizz
  | PrintBuzz
  | PrintFizzBuzz
  | PrintNumber Int
  deriving (Show)

type FizzBuzz = Loop FizzBuzzAction

evaluateLoop :: Loop a -> (a -> IO ()) -> IO ()
evaluateLoop End _ = pure ()
evaluateLoop (Step action loop) fn = fn action >> evaluateLoop loop fn

evaluateFizzBuzzAction :: FizzBuzzAction -> IO ()
evaluateFizzBuzzAction PrintFizz = putStrLn "fizz"
evaluateFizzBuzzAction PrintBuzz = putStrLn "buzz"
evaluateFizzBuzzAction PrintFizzBuzz = putStrLn "fizzbuzz"
evaluateFizzBuzzAction (PrintNumber n) = print n

evaluateFizzBuzz :: FizzBuzz -> IO ()
evaluateFizzBuzz fizzBuzz = evaluateLoop fizzBuzz evaluateFizzBuzzAction

run :: IO ()
run = evaluateFizzBuzz genFizzBuzz

genFizzBuzz :: FizzBuzz
genFizzBuzz =
  toFizzBuzz [1 .. 100000000]
  where
    toFizzBuzz [] = End
    toFizzBuzz (n : ns)
      | n `mod` 15 == 0 = Step PrintFizzBuzz (toFizzBuzz ns)
      | n `mod` 3 == 0 = Step PrintFizz (toFizzBuzz ns)
      | n `mod` 5 == 0 = Step PrintBuzz (toFizzBuzz ns)
      | otherwise = Step (PrintNumber n) (toFizzBuzz ns)

-- 6.2.1

run2 :: IO ()
run2 = do
  filePaths <- getArgs
  handleFiles filePaths $ \handle -> do
    foreachLine handle $ \line -> do
      putStrLn (map toUpper line)

handleFiles :: [FilePath] -> (Handle -> IO b) -> IO ()
handleFiles filePaths fileHandler =
  forM_ filePaths $ \filePath ->
    withFile filePath ReadMode fileHandler

concatFiles :: [FilePath] -> Handle -> IO ()
concatFiles filePaths destination = handleFiles filePaths (`copyFile` destination)

copyFile :: Handle -> Handle -> IO ()
copyFile source destination = copyFileWithConverter source destination id

copyFileWithConverter :: Handle -> Handle -> (String -> String) -> IO ()
copyFileWithConverter source destination converter =
  loop
  where
    loop = do
      isEof <- hIsEOF source
      if isEof
        then pure ()
        else do
          line <- hGetLine source
          hPutStrLn destination (converter line)
          loop

foreachLineWithAction :: Handle -> Handle -> (String -> IO String) -> IO ()
foreachLineWithAction source destination action = foreachLine source (action >=> hPutStrLn destination)

foreachLine :: Handle -> (String -> IO a) -> IO ()
foreachLine source action =
  loop
  where
    loop = do
      isEof <- hIsEOF source
      if isEof
        then pure ()
        else do
          line <- hGetLine source
          action line
          loop

-- 6.2.2

run3 :: IO ()
run3 = getArgs >>= (`foreachFileLine` putStrLn)

foreachFileLine :: [FilePath] -> (String -> IO a) -> IO ()
foreachFileLine filePaths lineHandler = handleFiles filePaths (`foreachLine` lineHandler)

-- 6.4.1

data Paper = Paper
  { paperId :: String,
    title :: String,
    author :: String,
    erdosNumber :: Int
  }
  deriving (Show)

instance Eq Paper where
  p1 == p2 = paperId p1 == paperId p2

instance Ord Paper where
  p1 <= p2 = erdosNumber p1 <= erdosNumber p2

class Triple a where
  triple :: a -> a

instance Triple Int where
  triple n = n * 3

instance Triple String where
  triple s = s ++ s ++ s

-- 6.4.2

newtype Name = Name String

newtype Path = Path String

data DefaultValues = DefaultValues {defaultName :: Name, defaultPath :: Path}

getDefaultName :: MonadReader DefaultValues m => m Name
getDefaultName = asks defaultName

getDefaultPath :: MonadReader DefaultValues m => m Path
getDefaultPath = asks defaultPath

newtype MyApp a = MyApp {unMyApp :: ReaderT DefaultValues IO a}

runMyApp :: DefaultValues -> MyApp a -> IO a
runMyApp defaultValues myApp = runReaderT (unMyApp myApp) defaultValues
