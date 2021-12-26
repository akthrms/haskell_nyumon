module Main06 where

-- 3.7.1

type Age = Integer

legalDrink :: Age -> Bool
legalDrink age
  | age >= 20 = True
  | otherwise = False

data AppErr = NewAppErr deriving (Show)

type AppResult a = Either AppErr a

safeHead :: [a] -> Either AppErr a
safeHead [] = Left NewAppErr
safeHead (x : xs) = Right x

-- 3.7.2

newtype NTIndexed a = NewNTIndexed
  { unNTIndexed :: (Integer, a)
  }
  deriving (Show)

x, y :: NTIndexed String
x = NewNTIndexed (10, "ten")
y = NewNTIndexed (12, "twelve")

main :: IO ()
main = do
  print $ NewNTIndexed (unNTIndexed x)
  print $ unNTIndexed (NewNTIndexed (2, "two"))
  print $ snd (unNTIndexed y)
