module Main04 where

-- 2.6.3

f :: Integer -> IO ()
f x = case x of
  n
    | even n -> putStrLn "even"
    | otherwise -> putStrLn "odd"

g :: (Integer, Bool) -> Integer
g x = case x of
  (x1, True) | even x1 -> x1 `div` 2
  (x1, _)
    | even x1 -> x1
    | otherwise -> x1 - 1

h :: (Integer, Bool) -> Integer
h x = case x of
  (x1, True) | (q, 0) <- x1 `divMod` 2 -> q
  (x1, _) -> x1

-- 2.6.4

h' :: (Integer, Bool) -> Integer
h' (x1, True) | (q, 0) <- x1 `divMod` 2 = q
h' (x1, _) = x1

main :: IO ()
main = do
  f 10
  f 11

  print $ g (10, True)
  print $ g (11, True)
  print $ g (10, False)

  print $ h (10, True)
  print $ h (11, True)
  print $ h (10, False)
