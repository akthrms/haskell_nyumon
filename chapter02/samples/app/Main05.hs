module Main05 where

-- 2.7.3

import Data.Maybe (fromJust, isNothing)

percentage :: (Eq a, Fractional a) => a -> a -> Maybe a
percentage k n
  | n == 0 = Nothing
  | otherwise = Just $ 100.0 * k / n

-- 2.7.4

percentage' :: (Eq b, Fractional b) => b -> b -> Either String b
percentage' k n
  | n == 0 = Left "Illegal division by zero"
  | otherwise = Right $ 100.0 * k / n

main :: IO ()
main = do
  print $ case percentage 20 50 of
    Nothing -> "UNKNOWN"
    Just x -> show x

  let p = percentage 20 50
  print $ if isNothing p then "UNKNOWN" else show (fromJust p)

  print $ maybe "UNKNOWN" show (percentage 20 50)

  print $ case percentage' 20 50 of
    Left e -> e
    Right x -> show x

  print $ either id show (percentage' 20 50)
