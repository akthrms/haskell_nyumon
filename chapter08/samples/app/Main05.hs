module Main05 where

import Control.Concurrent
  ( forkIO,
    newEmptyMVar,
    putMVar,
    takeMVar,
  )
import Control.Concurrent.STM
  ( atomically,
    newTVarIO,
    readTVar,
    writeTVar,
  )

-- 8.3.2

main :: IO ()
main = do
  putStrLn "Begin"
  mv1 <- newEmptyMVar
  mv2 <- newEmptyMVar

  account1 <- newTVarIO (10000 :: Int)
  account2 <- newTVarIO (10000 :: Int)

  let wait b = case b of
        0 -> pure 1
        1 -> pure 1
        n -> (+) <$> wait (n - 1) <*> wait (n - 2)

  forkIO $ do
    atomically $ do
      balance1 <- readTVar account1
      balance2 <- readTVar account2
      wait 35
      writeTVar account1 (balance1 + 1000)
      writeTVar account1 (balance2 - 1000)
    putMVar mv1 ()

  forkIO $ do
    atomically $ do
      wait 32
      balance1 <- readTVar account1
      balance2 <- readTVar account2
      writeTVar account1 (balance1 - 2000)
      writeTVar account1 (balance2 + 2000)
    putMVar mv2 ()

  takeMVar mv1
  takeMVar mv2

  balances <- atomically $ (,) <$> readTVar account1 <*> readTVar account2
  print balances
  putStrLn "Done"
