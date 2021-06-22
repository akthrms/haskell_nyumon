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
  getLine >>= pure . ("2つ目の入力: " ++) >>= putStrLn

run4 :: IO ()
run4 = do
  args <- getArgs
  print args

run5 :: IO ()
run5 = do
  xs <- getContents >>= pure . lines
  counter 0 xs

counter :: Int -> [String] -> IO ()
counter _ [] = pure ()
counter i ("up" : xs) = print (i + 1) >> counter (i + 1) xs
counter i ("down" : xs) = print (i - 1) >> counter (i - 1) xs
counter i (_ : xs) = counter i xs

interact' :: (String -> String) -> IO ()
interact' f = do
  s <- getContents
  putStr $ f s

run6 :: IO ()
run6 = interact' $ map toUpper

run7 :: IO ()
run7 = readFile "sample.txt" >>= putStrLn

run8 :: IO ()
run8 = readFile "sample.txt" >>= putStrLn . reverse

run9 :: IO ()
run9 = do
  s <- readFile "sample.txt"
  putStrLn $ take 5 s
  writeFile "sample.txt" "Hello, Lazy IO!"

run10 :: IO ()
run10 = do
  h <- openFile "sample.txe" ReadMode
  loop 0 h
  hClose h
  where
    loop i h = do
      eof <- hIsEOF h
      if not eof
        then do
          s <- hGetLine h
          putStrLn $ show i ++ ":" ++ s
          loop (i + 1) h
        else pure ()

run11 :: IO ()
run11 = do
  s <- B.readFile "sample"
  B.writeFile "sample" . B.map complement $ s

run12 :: IO ()
run12 = getPermissions "sample.txt" >>= setPermissions "sample.txt" . toReadableAndWritable

toReadableAndWritable :: Permissions -> Permissions
toReadableAndWritable p = p {readable = True, writable = True}

run13 :: IO ()
run13 = do
  current <- getCurrentDirectory
  findFiles [current ++ "/..", current] "target.txt" >>= print
  findFile [current ++ "/..", current] "target.txt" >>= print

run14 :: IO ()
run14 = do
  current <- getCurrentDirectory
  findFilesWith checkWritable [current ++ "/..", current] "target.txt" >>= print

checkWritable :: FilePath -> IO Bool
checkWritable filePath = getPermissions filePath >>= pure . writable

run15 :: IO ()
run15 =
  (readFile "dummy.txt" >>= putStrLn)
    `catch` ( \e ->
                putStrLn $ "readFile failure!!!: " ++ displayException (e :: SomeException)
            )

run16 :: IO ()
run16 =
  (readFile "dummy.txt" >>= putStrLn)
    `catch` ( \(e :: SomeException) ->
                putStrLn $ "readFile failure!!!: " ++ displayException e
            )

run17 :: IO ()
run17 =
  (readFile "dummy.txt" >>= putStrLn)
    `catch` ( \(e :: SomeException) ->
                putStrLn $ "readFile failure!!!: " ++ displayException e
            )
    `finally` (putStrLn "finalization")

catchZeroDiv :: ArithException -> IO Int
catchZeroDiv DivideByZero = pure 0
catchZeroDiv e = throwIO e

run18 :: IO ()
run18 = do
  n <- (pure $ 100 `div` 0) `catch` catchZeroDiv
  print n
  m <- (pure $! 100 `div` 0) `catch` catchZeroDiv
  print m

someOperation :: IO ()
someOperation = undefined

run19 :: IO ()
run19 =
  someOperation
    `catch` ( \(e :: ArithException) ->
                putStrLn $ "Catch ArithException: " ++ displayException e
            )
    `catch` ( \(e :: SomeException) ->
                putStrLn $ "Catch SomeException: " ++ displayException e
            )

run20 :: IO ()
run20 =
  someOperation
    `catches` [ Handler $ \(e :: ArithException) ->
                  putStrLn $ "Catch ArithException: " ++ displayException e,
                Handler $ \(e :: SomeException) ->
                  putStrLn $ "Catch SomeException: " ++ displayException e
              ]

run21 :: IO ()
run21 =
  bracket (openFile "dummy.txt" ReadMode) hClose $ \h -> do
    s <- hGetContents h
    putStrLn s

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

run22 :: IO ()
run22 = do
  (throwMyException 1 >>= putStrLn) `catch` printMyException
  (throwMyException 2 >>= putStrLn) `catch` printMyException
  (throwMyException 3 >>= putStrLn) `catch` printMyException
