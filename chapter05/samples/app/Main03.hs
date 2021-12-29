module Main03 where

import Control.Monad.Trans.State (State, get, put, runState)
import Data.List (sort)

-- 5.1.3

type Card = Int

type Score = Int

type Hand = [Card]

type Stock = [Card]

type Player = String

game :: State Stock [(Score, Hand, Player)]
game = do
  taroHand <- drawCards 5
  hanakoHand <- drawCards 5
  takashiHand <- drawCards 5
  yumiHand <- drawCards 5
  pure . reverse . sort $
    [ (sum taroHand, taroHand, "Taro"),
      (sum hanakoHand, hanakoHand, "Hanako"),
      (sum takashiHand, takashiHand, "Takashi"),
      (sum yumiHand, yumiHand, "Yumi")
    ]

drawCards :: Int -> State Stock Hand
drawCards n = do
  deck <- get
  put (drop n deck)
  pure (take n deck)

main :: IO ()
main = do
  print (runState game [1 .. 50])
