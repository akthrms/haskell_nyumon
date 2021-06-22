module Chapter05 where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM_, guard)
import qualified Control.Monad.Trans.Except as EX
import Control.Monad.Trans.Reader
  ( Reader,
    ask,
    asks,
    local,
    runReader,
  )
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

-- 5.2.1

run1 :: IO ()
run1 = do
  forM_ [1 .. 9] $ \x -> do
    forM_ [1 .. 9] $ \y -> do
      putStr $ show (x * y) ++ "\t"
    putStrLn ""

-- 5.3.2

data Gender = Man | Woman deriving (Show)

data Human = Human {name :: String, age :: Int, gender :: Gender} deriving (Show)

run2 :: IO ()
run2 = do
  print $ Human "Taro" 10 Man
  print $ Human <$> Just "Taro" <*> pure 10 <*> pure Man
  print $ Human <$> Just "Taro" <*> Nothing <*> pure Man

-- 5.3.3

assocs :: [(String, Integer)]
assocs = [("hiratara", 39), ("shu1", 0), ("masaharu", 32)]

run3 :: IO ()
run3 = do
  print $ lookup "homma" assocs <|> lookup "hiratara" assocs
  print $ do
    age <- lookup "homma" assocs <|> lookup "hiratara" assocs
    guard $ age < 20
    pure age

-- 5.4.1

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv k n
  | n == 0 = Nothing
  | otherwise = Just $ k `div` n

calc :: Integer -> Maybe Integer
calc n = do
  x <- 100 `safeDiv` n
  -- y <- 100 `safeDiv` (x - 1)
  -- pure y
  100 `safeDiv` (x - 1)

safeDiv' :: Integer -> Integer -> Either String Integer
safeDiv' k n
  | n == 0 = Left $ "Illegal division by zero. k: " ++ show k
  | otherwise = Right $ k `div` n

calc' :: Integer -> Either String Integer
calc' n = do
  x <- 100 `safeDiv'` n
  100 `safeDiv'` (x - 1)

-- >>> calc' 50
-- Right 100
-- >>> calc' 0
-- Left "Illegal division by zero. k: 100"

-- 5.4.2

safeDiv'' :: Integer -> Integer -> EX.Except String Integer
safeDiv'' k n
  | n == 0 = EX.throwE $ "Illegal division by zero. k: " ++ show k
  | otherwise = pure $ k `div` n

calc'' :: Integer -> Either String Integer
calc'' n = EX.runExcept $ do
  EX.catchE
    ( do
        x <- 100 `safeDiv''` n
        100 `safeDiv''` (x - 1)
    )
    (\_ -> pure 0)

-- 5.5.1

run4 :: IO ()
run4 = print $ runReader readRound 1.5013232

readRound :: Reader Double Int
readRound = do round <$> ask

data PowerEnv = PowerEnv {powerEnergy :: !Double, powerSaveMode :: !Bool}

consume :: Reader PowerEnv Double
consume = do
  energy <- asks powerEnergy
  saveMode <- asks powerSaveMode
  let consumption = if saveMode then energy / 10.0 else energy
  pure consumption

run5 :: IO ()
run5 = print $ runReader consume $ PowerEnv 10.0 True

testRun :: PowerEnv -> Double
testRun env = (`runReader` env) $ do
  cons1 <- consume
  cons2 <- consume
  consOther <- local (\e -> e {powerSaveMode = True}) $ do
    cons3 <- consume
    cons4 <- consume
    pure $ cons3 + cons4
  pure $ cons1 + cons2 + consOther

-- >>> testRun $ PowerEnv 100.0 False
-- 220.0
-- >>> testRun $ PowerEnv 80.0 False
-- 176.0
-- >>> testRun $ PowerEnv 100.0 True
-- 40.0
