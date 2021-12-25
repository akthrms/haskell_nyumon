module Main01 where

import System.Environment (getEnv)

-- 2.5

main :: IO ()
main = do
  let title = "Current User"
  user <- getEnv "USER"
  putStrLn title
  putStrLn user
