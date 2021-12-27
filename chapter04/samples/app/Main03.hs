module Main03 where

-- 4.3.3

main :: IO ()
main = do
  xs <- getContents >>= pure . lines
  counter 0 xs

counter :: Int -> [String] -> IO ()
counter _ [] = pure ()
counter i ("up" : xs) = print (i + 1) >> counter (i + 1) xs
counter i ("down" : xs) = print (i - 1) >> counter (i - 1) xs
counter i (_ : xs) = counter i xs

interact :: (String -> String) -> IO ()
interact f = do
  s <- getContents
  putStrLn $ f s

main' :: IO ()
main' = readFile "sample.txt" >>= putStrLn

main'' :: IO ()
main'' = readFile "sample.txt" >>= putStrLn . reverse
