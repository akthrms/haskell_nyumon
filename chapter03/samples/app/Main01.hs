module Main01 where

-- 3.2.1

intToArray :: Int -> Int -> [Int]
intToArray = replicate

main :: IO ()
main = do
  print $ intToArray 3 4
