module Main01 where

-- 4.1.3

main :: IO ()
main =
  getLine >>= \x ->
    getLine >>= \y ->
      putStrLn ("1つ目の入力: " ++ x) >> putStrLn ("2つ目の入力: " ++ y)

main' :: IO ()
main' = do
  x <- getLine
  y <- getLine
  putStrLn $ "1つ目の入力: " ++ x
  putStrLn $ "2つ目の入力: " ++ y
