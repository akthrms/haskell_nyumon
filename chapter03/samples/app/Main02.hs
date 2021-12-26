module Main02 where

-- 3.4.3

data CmdOption
  = COptInt Integer
  | COptBool Bool
  | COptStr String
  deriving (Show)

coptToInt :: CmdOption -> Int
coptToInt (COptInt n) = fromIntegral n
coptToInt (COptStr x) = read x
coptToInt (COptBool True) = 1
coptToInt (COptBool False) = 0

main :: IO ()
main = do
  print $ coptToInt $ COptInt 120
  print $ coptToInt $ COptStr "0x78"
