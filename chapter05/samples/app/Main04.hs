module Main04 where

import Control.Monad (forM_)

-- 5.2.1

main :: IO ()
main = do
  forM_ [1 .. 9] $ \x -> do
    forM_ [1 .. 9] $ \y -> do
      putStr $ show (x * y) ++ "\t"
    putStrLn ""
