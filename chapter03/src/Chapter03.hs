module Chapter03 where

import qualified Data.List as L
import qualified Data.Map.Strict as M

intToArray :: Int -> Int -> [Int]
-- intToArray n m = take m $ repeat n
intToArray n m = replicate m n

run :: IO ()
run = do
  print $ intToArray 3 4

data CmdOption
  = CmdOptionInteger Integer
  | CmdOptionBool Bool
  | CmdOptionString String
  deriving (Show)

-- >>> cmdOptionToInt $ CmdOptionInteger 120
-- 120
-- >>> cmdOptionToInt $ CmdOptionString "0x78"
-- 120

cmdOptionToInt :: CmdOption -> Int
cmdOptionToInt (CmdOptionInteger n) = fromIntegral n
cmdOptionToInt (CmdOptionBool True) = 1
cmdOptionToInt (CmdOptionBool False) = 0
cmdOptionToInt (CmdOptionString s) = read s

data LazyAndStrict = LazyAndStrict {isLazy :: Int, isStrict :: !Int}

data Employee = NewEmployee
  { employeeAge :: Integer,
    employeeIsManager :: Bool,
    employeeName :: String
  }
  deriving (Show)

employee :: Employee
employee = NewEmployee {employeeAge = 39, employeeIsManager = False, employeeName = "Subhash Khot"}

employee' :: Employee
employee' = employee {employeeIsManager = True, employeeAge = employeeAge employee + 1}

data TreeDict k v
  = TreeDictEmpty
  | TreeDictNode k v (TreeDict k v) (TreeDict k v)
  deriving (Show)

insert :: Ord k => k -> v -> TreeDict k v -> TreeDict k v
insert k v TreeDictEmpty = TreeDictNode k v TreeDictEmpty TreeDictEmpty
insert k v (TreeDictNode k' v' l r)
  | k < k' = TreeDictNode k' v' (insert k v l) r
  | k > k' = TreeDictNode k' v' r (insert k v r)
  | otherwise = TreeDictNode k v l r

--- >>> dict
-- TreeDictNode "masahiko" 63 (TreeDictNode "hiratara" 39 TreeDictEmpty TreeDictEmpty) (TreeDictNode "shu1" 0 TreeDictEmpty TreeDictEmpty)

dict :: TreeDict String Integer
dict =
  insert "hiratara" 39
    . insert "shu1" 0
    . insert "masahiko" 63
    $ TreeDictEmpty

-- >>> lookup' "hiratara" dict
-- Just 39
-- >>> lookup' "pinnyu" dict
-- Nothing

lookup' :: Ord k => k -> TreeDict k v -> Maybe v
lookup' _ TreeDictEmpty = Nothing
lookup' k (TreeDictNode k' v' l r)
  | k < k' = lookup' k l
  | k > k' = lookup' k r
  | otherwise = Just v'

-- >>> M.lookup "shu1" dict'
-- Just 0

dict' :: M.Map String Integer
dict' =
  M.insert "hiratara" 39
    . M.insert "shu1" 0
    . M.insert "masahiko" 63
    $ M.empty

type Age = Integer

legalDrink :: Age -> Bool
legalDrink age
  | age >= 20 = True
  | otherwise = False

run1 :: IO ()
run1 = do
  let age = 24 :: Age
      n = -1 :: Integer
  print $ legalDrink age
  print $ legalDrink n

data AppErr = AppErr deriving (Show)

type AppResult a = Either AppErr a

safeHead :: [a] -> AppResult a
safeHead [] = Left AppErr
safeHead (x : _) = Right x

run2 :: IO ()
run2 = do
  print $ safeHead ([1, 2, 3] :: [Int])
  print $ safeHead ([] :: [Int])

newtype NewTypeIndexed a = NewTypeIndexed {unNewTypeIndexed :: (Integer, a)} deriving (Show)

x :: NewTypeIndexed String
x = NewTypeIndexed (10, "ten")

y :: NewTypeIndexed String
y = NewTypeIndexed (12, "twelve")

run3 :: IO ()
run3 = do
  print $ NewTypeIndexed $ unNewTypeIndexed x
  print $ unNewTypeIndexed $ NewTypeIndexed (2, "two")
  print $ snd $ unNewTypeIndexed y

data Dog = Dog deriving (Show)

data Cat = Cat deriving (Show)

newtype Human = Human String deriving (Show)

class Greeting a where
  name :: a -> String

  hello :: a -> String
  hello _ = "..."

  bye :: a -> String
  bye _ = "..."

-- >>> hello $ Human "takashi"
-- "Hi, I'm takashi."
-- >>> hello Dog
-- "Bark!"
-- >>> hello Cat
-- "..."
-- >>> bye $ Human "takashi"
-- "See you."
-- >>> bye Dog
-- "..."
-- >>> bye Cat
-- "Meow..."

instance Greeting Dog where
  name _ = "a dog"

  hello _ = "Bark!"

instance Greeting Cat where
  name _ = "a cat"

  bye _ = "Meow..."

instance Greeting Human where
  name (Human n) = n

  hello h = "Hi, I'm " ++ name h ++ "."

  bye _ = "See you."

sayHello :: Greeting a => a -> IO ()
sayHello x = putStrLn $ hello x

class Greeting a => Laughing a where
  laugh :: a -> String

instance Laughing Human where
  laugh _ = "Zehahahah...!!"

leaveWithLaugh :: Laughing a => a -> IO ()
leaveWithLaugh x = do
  putStrLn $ bye x
  putStrLn $ laugh x

liftGreet :: (a -> String) -> ([a] -> String)
liftGreet f = L.intercalate "\n" . map f

instance Greeting a => Greeting [a] where
  name = liftGreet name

  hello = liftGreet hello

  bye = liftGreet bye

class Breeding a where
  breed :: String -> a

instance Breeding Human where
  breed = Human

clone :: (Breeding a, Greeting a) => a -> a
clone x = breed (name x) `asTypeOf` x

run4 :: IO ()
run4 = do
  print $ hello $ clone $ Human "takashi"

newtype BString = BString {unBString :: String} deriving (Show)

instance Breeding BString where
  breed = BString

run5 :: IO ()
run5 = do
  print (breed "a raw string" :: BString)
