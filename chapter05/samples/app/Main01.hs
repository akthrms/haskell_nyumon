module Main01 where

-- 5.1.1

f :: Maybe Int
f = Just 10 >>= \x -> Nothing >> pure (x * 2)

f' :: Maybe Int
f' = do
  x <- Just 10
  Nothing
  pure (x * 2)

main :: IO ()
main = do
  print f
  print f'
