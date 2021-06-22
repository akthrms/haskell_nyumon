module Chapter05 where

import Control.Monad.Trans.State (State, get, put, runState)
import Data.List (sort)
import System.Random.Shuffle (shuffleM)

-- 5.1.1

f :: Maybe Int
f = do
  x <- Just 10
  Nothing
  pure $ x * 2

f' :: Maybe Int
-- f' = Just 10 >>= \x -> Nothing >>= \_ -> pure $ x * 2
f' = Just 10 >>= \x -> Nothing >> pure (x * 2)

-- 5.1.2

type Category = String

type Name = String

type Price = Integer

type Menu = [(Category, [(Name, Price)])]

type Item = (Category, Name, Price)

menu :: Menu
menu =
  [ ("Food", [("Hamburger", 120), ("FrenchFries", 100)]),
    ("Drink", [("Cola", 80), ("Tea", 100)])
  ]

getItemWithoutMonad :: Menu -> Category -> Name -> Maybe Item
getItemWithoutMonad menu category name =
  case lookup category menu of
    Just subMenu -> case lookup name subMenu of
      Just price -> Just (category, name, price)
      Nothing -> Nothing
    Nothing -> Nothing

getItemWithMonad :: Menu -> Category -> Name -> Maybe Item
getItemWithMonad menu category name = do
  subMenu <- lookup category menu
  price <- lookup name subMenu
  pure (category, name, price)

-- 5.1.3

type Card = Int

type Score = Int

type Hand = [Card]

type Stock = [Card]

type Player = String

game :: [Card] -> [(Score, Hand, Player)]
game deck =
  reverse . sort $
    [ (sum taroHand, taroHand, "Taro"),
      (sum hanakoHand, hanakoHand, "Hanako"),
      (sum takashiHand, takashiHand, "Takashi"),
      (sum yumiHand, yumiHand, "Yumi")
    ]
  where
    (taroHand, deck2) = splitAt 5 deck
    (hanakoHand, deck3) = splitAt 5 deck2
    (takashiHand, deck4) = splitAt 5 deck3
    (yumiHand, deck5) = splitAt 5 deck4

drawCards :: Int -> State Stock Hand
drawCards n = do
  deck <- get
  put $ drop n deck
  pure $ take n deck

gameWithState :: State Stock [(Score, Hand, Player)]
gameWithState = do
  taroHand <- drawCards 5
  hanakoHand <- drawCards 5
  takashiHand <- drawCards 5
  yumiHand <- drawCards 5
  pure $
    reverse . sort $
      [ (sum taroHand, taroHand, "Taro"),
        (sum hanakoHand, hanakoHand, "Hanako"),
        (sum takashiHand, takashiHand, "Takashi"),
        (sum yumiHand, yumiHand, "Yumi")
      ]

run :: IO ()
run = do
  print $ game [1 .. 50]
  print $ runState gameWithState [1 .. 50]

runGame :: IO ()
runGame = do
  deck <- shuffleM [1 .. 50]
  print $ runState gameWithState deck
