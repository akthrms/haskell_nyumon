{-# LANGUAGE ScopedTypeVariables #-}

module Main07 where

import Control.Exception
  ( ArithException (DivideByZero),
    Exception (displayException),
    Handler (Handler),
    SomeException (SomeException),
    catch,
    catches,
    finally,
    throwIO,
  )

-- 4.5.1

main :: IO ()
main =
  (readFile "dummyFileName" >>= putStrLn)
    `catch` (\(e :: SomeException) -> putStrLn $ "readFile failure: " ++ displayException e)
    `finally` putStrLn "finalization"

catchZeroDiv :: ArithException -> IO Int
catchZeroDiv DivideByZero = pure 0
catchZeroDiv e = throwIO e

main' :: IO ()
main' = do
  n1 <- (pure $ 100 `div` 0) `catch` catchZeroDiv
  print n1
  n2 <- (pure $! 100 `div` 0) `catch` catchZeroDiv
  print n2

someOperation :: IO ()
someOperation = undefined

main'' :: IO ()
main'' =
  someOperation
    `catches` [ Handler $ \(e :: ArithException) -> putStrLn $ "Catch ArithException: " ++ displayException e,
                Handler $ \(e :: SomeException) -> putStrLn $ "Catch SomeException: " ++ displayException e
              ]
