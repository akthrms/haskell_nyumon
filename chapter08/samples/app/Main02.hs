module Main02 where

import Control.Concurrent
  ( forkIO,
    myThreadId,
    newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
  )

-- 8.2.2

main :: IO ()
main = do
  m <- newEmptyMVar

  forkIO $ do
    tid <- myThreadId
    putStrLn (show tid ++ ": doing ... heavy ... task ...")
    threadDelay 2000000
    putMVar m ()

  takeMVar m
  putStrLn "Done"
