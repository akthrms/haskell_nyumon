{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter04 where

import Control.Exception
  ( ArithException (DivideByZero),
    Exception (displayException),
    Handler (Handler),
    SomeException,
    bracket,
    catch,
    catches,
    finally,
    throwIO,
  )
import Data.Bits (Bits (complement))
import qualified Data.ByteString as B
import Data.Char (toUpper)
import Data.Typeable (Typeable)
import System.Directory
  ( Permissions (readable, writable),
    findFile,
    findFiles,
    findFilesWith,
    getCurrentDirectory,
    getPermissions,
    setPermissions,
  )
import System.Environment (getArgs)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    hGetLine,
    hIsEOF,
    openFile,
  )

-- 4.1.3

run :: IO ()
run =
  getLine >>= \x ->
    getLine >>= \y ->
      putStrLn ("1つ目の入力: " ++ x) >> putStrLn ("2つ目の入力: " ++ y)

run2 :: IO ()
run2 = do
  x <- getLine
  y <- getLine
  putStrLn $ "1つ目の入力: " ++ x
  putStrLn $ "2つ目の入力: " ++ y

run3 :: IO ()
run3 = do
  x <- getLine
  putStrLn $ "1つ目の入力: " ++ x
  -- getLine >>= pure . ("2つ目の入力: " ++) >>= putStrLn
  getLine >>= putStrLn . ("2つ目の入力: " ++)

-- 4.2.1

run4 :: IO ()
run4 = do
  args <- getArgs
  print args

-- 4.3.3

run5 :: IO ()
run5 = do
  -- operations <- getContents >>= pure . lines
  operations <- lines <$> getContents
  counter 0 operations

counter :: Int -> [String] -> IO ()
counter _ [] = pure ()
counter i ("up" : operations) = print (i + 1) >> counter (i + 1) operations
counter i ("down" : operations) = print (i - 1) >> counter (i - 1) operations
counter i (_ : operations) = counter i operations

interact' :: (String -> String) -> IO ()
interact' f = do
  s <- getContents
  putStr $ f s

run6 :: IO ()
run6 = interact' $ map toUpper

-- 4.3.4

run7 :: IO ()
run7 = do
  handle <- openFile "sample.txe" ReadMode
  loop 0 handle
  hClose handle
  where
    loop i handle = do
      isEof <- hIsEOF handle
      if not isEof
        then do
          s <- hGetLine handle
          putStrLn $ show i ++ ":" ++ s
          loop (i + 1) handle
        else pure ()

-- 4.3.5

run8 :: IO ()
run8 = do
  bs <- B.readFile "sample"
  B.writeFile "sample" . B.map complement $ bs

-- 4.4.2

run9 :: IO ()
run9 = getPermissions "sample.txt" >>= setPermissions "sample.txt" . toReadableAndWritable

toReadableAndWritable :: Permissions -> Permissions
toReadableAndWritable permissions = permissions {readable = True, writable = True}

run10 :: IO ()
run10 = do
  currentDirectory <- getCurrentDirectory
  findFiles [currentDirectory ++ "/..", currentDirectory] "target.txt" >>= print
  findFile [currentDirectory ++ "/..", currentDirectory] "target.txt" >>= print

run11 :: IO ()
run11 = do
  currentDirectory <- getCurrentDirectory
  findFilesWith checkWritable [currentDirectory ++ "/..", currentDirectory] "target.txt" >>= print
  where
    -- checkWritable filePath = getPermissions filePath >>= pure . writable
    checkWritable filePath = writable <$> getPermissions filePath

-- 4.5.1

readDummyFile :: IO ()
readDummyFile = readFile "dummy.txt" >>= putStrLn

putErrorMessage :: SomeException -> IO ()
putErrorMessage e = putStrLn $ "readFile failure!!!: " ++ displayException (e :: SomeException)

run12 :: IO ()
run12 = readDummyFile `catch` putErrorMessage

putErrorMessage' :: SomeException -> IO ()
putErrorMessage' (e :: SomeException) = putStrLn $ "readFile failure!!!: " ++ displayException e

run13 :: IO ()
run13 = readDummyFile `catch` putErrorMessage'

run14 :: IO ()
run14 = readDummyFile `catch` putErrorMessage' `finally` putStrLn "finalization"

catchZeroDiv :: ArithException -> IO Int
catchZeroDiv DivideByZero = pure 0
catchZeroDiv e = throwIO e

-- >>> (pure $ 100 `div` 0) `catch` catchZeroDiv
-- divide by zero
-- >>> (pure $! 100 `div` 0) `catch` catchZeroDiv
-- 0

someOperation :: IO ()
someOperation = undefined

catchArithException :: ArithException -> IO ()
catchArithException (e :: ArithException) = putStrLn $ "Catch ArithException: " ++ displayException e

catchSomeException :: SomeException -> IO ()
catchSomeException (e :: SomeException) = putStrLn $ "Catch SomeException: " ++ displayException e

run15 :: IO ()
run15 = someOperation `catch` catchArithException `catch` catchSomeException

run16 :: IO ()
run16 = someOperation `catches` [Handler catchArithException, Handler catchSomeException]

-- 4.5.2

run17 :: IO ()
run17 =
  bracket (openFile "dummy.txt" ReadMode) hClose $ \handle -> do
    s <- hGetContents handle
    putStrLn s

-- 4.5.3

data MyException
  = FirstError
  | SecondError
  deriving (Show, Typeable)

instance Exception MyException

printMyException :: MyException -> IO ()
printMyException FirstError = putStrLn "Catch FirstError"
printMyException SecondError = putStrLn "Catch SecondError"

throwMyException :: Int -> IO String
throwMyException 1 = throwIO FirstError
throwMyException 2 = throwIO SecondError
throwMyException n = pure $ "Value = " ++ show n

run18 :: IO ()
run18 = do
  (throwMyException 1 >>= putStrLn) `catch` printMyException
  (throwMyException 2 >>= putStrLn) `catch` printMyException
  (throwMyException 3 >>= putStrLn) `catch` printMyException
