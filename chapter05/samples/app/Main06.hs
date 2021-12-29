module Main06 where

import qualified Control.Monad.Trans.Except as EX

-- 5.4.1

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv k n
  | n == 0 = Nothing
  | otherwise = Just (k `div` n)

calc :: Integer -> Maybe Integer
calc n = do
  x <- 100 `safeDiv` n
  100 `safeDiv` (x - 1)

safeDiv' :: Integer -> Integer -> Either String Integer
safeDiv' k n
  | n == 0 = Left ("Illegal division by zero. k: " ++ show k)
  | otherwise = Right (k `div` n)

calc' :: Integer -> Either String Integer
calc' n = do
  x <- 100 `safeDiv'` n
  100 `safeDiv'` (x - 1)

-- 5.4.2

safeDiv'' :: Integer -> Integer -> EX.Except String Integer
safeDiv'' k n
  | n == 0 = EX.throwE ("Illegal division by zero. k: " ++ show k)
  | otherwise = pure (k `div` n)

calc'' :: Integer -> Either String Integer
calc'' n = EX.runExcept $ do
  EX.catchE
    ( do
        x <- 100 `safeDiv''` n
        100 `safeDiv''` (x -1)
    )
    (\_ -> pure 0)

main :: IO ()
main = do
  print (calc 50)
  print (calc 0)

  print (calc' 50)
  print (calc' 0)

  print (calc'' 50)
  print (calc'' 0)
