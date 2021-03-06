module Main04 where

import Control.Concurrent
  ( forkIO,
    newMVar,
    putMVar,
    takeMVar,
    threadDelay,
  )

-- 8.3.1

main :: IO ()
main = do
  putStrLn "Begin"
  account1 <- newMVar 10000
  account2 <- newMVar 10000

  forkIO $ do
    balance1 <- takeMVar account1
    threadDelay 1000000
    balance2 <- takeMVar account2
    putMVar account1 (balance1 + 1000)
    putMVar account2 (balance2 - 1000)

  forkIO $ do
    balance2 <- takeMVar account2
    threadDelay 1000000
    balance1 <- takeMVar account1
    putMVar account2 (balance2 + 2000)
    putMVar account1 (balance1 - 2000)

  threadDelay 2000000
  balance1 <- takeMVar account1
  balance2 <- takeMVar account2
  print (balance1, balance2)
  putStrLn "Done"
