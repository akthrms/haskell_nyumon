module Chapter05 where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception.Lifted (bracket)
import Control.Monad (forM_, guard, unless)
import Control.Monad.Except
  ( Except,
    MonadError (catchError, throwError),
    MonadTrans (lift),
    forM_,
    guard,
    runExcept,
    runExceptT,
    unless,
  )
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
  ( Except,
    catchE,
    runExcept,
    runExceptT,
    throwE,
  )
import Control.Monad.Trans.Reader
  ( Reader,
    ReaderT (runReaderT),
    ask,
    asks,
    local,
    runReader,
  )
import Control.Monad.Trans.State
  ( State,
    evalStateT,
    get,
    modify,
    put,
    runState,
  )
import Control.Monad.Writer
  ( MonadTrans (lift),
    MonadWriter (tell),
    WriterT (runWriterT),
    forM_,
    guard,
    unless,
  )
import Data.Array.ST
  ( STUArray,
    getElems,
    newListArray,
    readArray,
    writeArray,
  )
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (sort)
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )
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

-- >>> fst $ runState gameWithState [1 .. 50]
-- [(90,[16,17,18,19,20],"Yumi"),(65,[11,12,13,14,15],"Takashi"),(40,[6,7,8,9,10],"Hanako"),(15,[1,2,3,4,5],"Taro")]
-- >>> snd $ runState gameWithState [1 .. 50]
-- [21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50]

runGame :: IO ()
runGame = do
  deck <- shuffleM [1 .. 50]
  print $ runState gameWithState deck

-- 5.2.1

run :: IO ()
run = do
  forM_ [1 .. 9] $ \x -> do
    forM_ [1 .. 9] $ \y -> do
      putStr $ show (x * y) ++ "\t"
    putStrLn ""

-- 5.3.2

data Gender = Man | Woman deriving (Show)

data Human = Human {name :: String, age :: Int, gender :: Gender} deriving (Show)

-- >>> Human "Taro" 10 Man
-- Human {name = "Taro", age = 10, gender = Man}
-- >>> Human <$> Just "Taro" <*> pure 10 <*> pure Man
-- Just (Human {name = "Taro", age = 10, gender = Man})
-- >>> Human <$> Just "Taro" <*> Nothing <*> pure Man
-- Nothing

-- 5.3.3

assocs :: [(String, Integer)]
assocs = [("hiratara", 39), ("shu1", 0), ("masaharu", 32)]

run2 :: IO ()
run2 = do
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

safeDiv'' :: Integer -> Integer -> Except String Integer
safeDiv'' k n
  | n == 0 = throwE $ "Illegal division by zero. k: " ++ show k
  | otherwise = pure $ k `div` n

calc'' :: Integer -> Either String Integer
calc'' n = runExcept $ do
  catchE
    ( do
        x <- 100 `safeDiv''` n
        100 `safeDiv''` (x - 1)
    )
    (\_ -> pure 0)

-- 5.5.1

run3 :: IO ()
run3 = print $ runReader readRound 1.5013232

readRound :: Reader Double Int
-- readRound = do
--   x <- ask
--   pure $ round x
readRound = do round <$> ask

data PowerEnv = PowerEnv {powerEnergy :: !Double, powerSaveMode :: !Bool}

consume :: Reader PowerEnv Double
consume = do
  energy <- asks powerEnergy
  saveMode <- asks powerSaveMode
  let consumption = if saveMode then energy / 10.0 else energy
  pure consumption

-- >>> runReader consume $ PowerEnv 10.0 True
-- 1.0

testRun :: PowerEnv -> Double
testRun powerEnv = (`runReader` powerEnv) $ do
  consumption1 <- consume
  consumption2 <- consume
  consumptionOther <- local (\pe -> pe {powerSaveMode = True}) $ do
    consumption3 <- consume
    consumption4 <- consume
    pure $ consumption3 + consumption4
  pure $ consumption1 + consumption2 + consumptionOther

-- >>> testRun $ PowerEnv 100.0 False
-- 220.0
-- >>> testRun $ PowerEnv 80.0 False
-- 176.0
-- >>> testRun $ PowerEnv 100.0 True
-- 40.0

-- 5.6.1

procCount :: Integer
procCount = runST $ do
  n <- newSTRef 0

  forM_ [1 .. 10] $ \i -> do
    modifySTRef n (+ 1)

  readSTRef n

-- >>> procCount
-- 10

-- 5.6.2

doubleArray :: [Double]
doubleArray = runST $ do
  arr <- newListArray (0, 4) [1 .. 5] :: ST s (STUArray s Int Double)
  x <- readArray arr 2
  writeArray arr 2 (x * 10.0)
  getElems arr

-- 5.7

points :: [(Integer, Integer)]
points = do
  x <- [1 .. 3]
  y <- [1 .. 3]
  pure (x, y)

-- >>> points
-- [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]

-- 5.7.1

orderedPoints :: [(Integer, Integer)]
orderedPoints = do
  x <- [1 .. 3]
  y <- [1 .. 3]
  guard (x < y)
  pure (x, y)

-- >>> orderedPoints
-- [(1,2),(1,3),(2,3)]

-- 5.7.2

-- >>> [(x, y) | x <- [1 .. 3], y <- [1 .. 3], x < y]
-- [(1,2),(1,3),(2,3)]

monadHead :: MonadFail m => [a] -> m a
monadHead xs = do
  (x : _) <- pure xs
  pure x

-- >>> monadHead [] :: Maybe Int
-- Nothing
-- >>> monadHead [] :: [Int]
-- []

-- 5.8.2

data Env = Env {envX :: !Integer, envY :: !Integer}

sumEnv :: ReaderT Env IO Integer
sumEnv = do
  x <- asks envX
  y <- asks envY
  pure $ x + y

-- >>> runReaderT sumEnv (Env 10 20)
-- 30

sumEnvIO :: ReaderT Env IO Integer
sumEnvIO = do
  x <- asks envX
  lift $ putStrLn $ "x = " ++ show x
  y <- asks envY
  lift $ putStrLn $ "y = " ++ show y
  pure $ x + y

-- >>> runReaderT sumEnvIO (Env 10 20)
-- 30

newtype Env' = Env' {envCount :: IORef Int}

countUp :: Int -> ReaderT Env' IO ()
countUp n = do
  ref <- asks envCount
  lift $ modifyIORef ref (+ n)

count :: ReaderT Env' IO Int
count = asks envCount >>= lift . readIORef

sum10 :: ReaderT Env' IO ()
sum10 = do
  forM_ [1 .. 10] $ \i -> do
    countUp i
    n <- count
    lift $ putStrLn $ "sum = " ++ show n

run4 :: IO ()
run4 = do
  ref <- newIORef 0
  runReaderT sum10 (Env' ref)
  readIORef ref >>= print

-- 5.8.3

run5 :: IO ()
run5 = do
  result <- (`evalStateT` 0) $ runExceptT loop
  case result of
    Right _ -> pure ()
    Left e -> putStrLn e
  where
    loop = do
      i <- state get
      unless (i < 3) $ throwE "Too much failure"
      operation <- io getLine
      if operation == "end"
        then pure ()
        else do
          state $ modify (+ 1)
          loop
    io = lift . lift
    state = lift

-- 5.8.6

mtlSample :: Either String ((), String)
mtlSample = runExcept $
  runWriterT $ do
    tell "Start\n"
    (`catchError` handler) $ do
      tell "In the block\n"
      _ <- throwError "some exception"
      tell "Never reach here\n"
    tell "End\n"
  where
    handler :: String -> WriterT String (Except String) ()
    handler e = tell $ "Caught the exception: " ++ e ++ "\n"

-- >>> mtlSample
-- Right ((),"Start\nCaught the exception: some exception\nEnd\n")

run6 :: IO ()
run6 = (`runReaderT` "sample.txt") $ do
  bracket open close $ \handle -> do
    contents <- lift $ hGetContents handle
    lift $ print $ length contents
  where
    open = do
      filePath <- ask
      lift $ openFile filePath ReadMode
    close = lift . hClose
