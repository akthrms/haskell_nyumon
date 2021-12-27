{-# LANGUAGE DeriveDataTypeable #-}

module Main09 where

import Control.Exception (Exception, catch, throwIO)
import Data.Typeable (Typeable)

-- 4.5.3

data MyException = FirstError | SecondError deriving (Show, Typeable)

instance Exception MyException

printMyException :: MyException -> IO ()
printMyException FirstError = putStrLn "Catch FirstError"
printMyException SecondError = putStrLn "Catch SecondError"

throwMyException :: Int -> IO String
throwMyException 1 = throwIO FirstError
throwMyException 2 = throwIO SecondError
throwMyException x = pure $ "Value = " ++ show x

main :: IO ()
main = do
  (throwMyException 1 >>= putStrLn) `catch` printMyException
  (throwMyException 2 >>= putStrLn) `catch` printMyException
  (throwMyException 3 >>= putStrLn) `catch` printMyException
