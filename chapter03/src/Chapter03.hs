module Chapter03 where

import qualified Data.List as L
import qualified Data.Map.Strict as M

-- 3.2.1

intToArray :: Int -> Int -> [Int]
-- intToArray n m = take m $ repeat n
intToArray n m = replicate m n

run :: IO ()
run = do
  print $ intToArray 3 4

-- 3.4.3

data CmdOption
  = CmdOptionInteger Integer
  | CmdOptionBool Bool
  | CmdOptionString String
  deriving (Show)

cmdOptionToInt :: CmdOption -> Int
cmdOptionToInt (CmdOptionInteger n) = fromIntegral n
cmdOptionToInt (CmdOptionBool True) = 1
cmdOptionToInt (CmdOptionBool False) = 0
cmdOptionToInt (CmdOptionString s) = read s

-- >>> cmdOptionToInt $ CmdOptionInteger 120
-- 120
-- >>> cmdOptionToInt $ CmdOptionString "0x78"
-- 120

-- 3.4.4

data LazyAndStrict = LazyAndStrict {isLazy :: Int, isStrict :: !Int}

-- 3.5

data Employee = NewEmployee
  { employeeAge :: Integer,
    employeeIsManager :: Bool,
    employeeName :: String
  }
  deriving (Show)

employee :: Employee
employee = NewEmployee {employeeAge = 39, employeeIsManager = False, employeeName = "Subhash Khot"}

-- 3.5.2

employee' :: Employee
employee' = employee {employeeIsManager = True, employeeAge = employeeAge employee + 1}

-- 3.6.1

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

dict :: TreeDict String Integer
dict =
  insert "hiratara" 39
    . insert "shu1" 0
    . insert "masahiko" 63
    $ TreeDictEmpty

-- >>> dict
-- TreeDictNode "masahiko" 63 (TreeDictNode "hiratara" 39 TreeDictEmpty TreeDictEmpty) (TreeDictNode "shu1" 0 TreeDictEmpty TreeDictEmpty)

lookup' :: Ord k => k -> TreeDict k v -> Maybe v
lookup' _ TreeDictEmpty = Nothing
lookup' k (TreeDictNode k' v' l r)
  | k < k' = lookup' k l
  | k > k' = lookup' k r
  | otherwise = Just v'

-- >>> lookup' "hiratara" dict
-- Just 39
-- >>> lookup' "pinnyu" dict
-- Nothing

dict' :: M.Map String Integer
dict' =
  M.insert "hiratara" 39
    . M.insert "shu1" 0
    . M.insert "masahiko" 63
    $ M.empty

-- >>> M.lookup "shu1" dict'
-- Just 0

-- 3.7.1

type Age = Integer

legalDrink :: Age -> Bool
legalDrink age
  | age >= 20 = True
  | otherwise = False

-- >>> legalDrink (24 :: Age)
-- True
-- >>> legalDrink (-1 :: Integer)
-- False

data AppErr = AppErr deriving (Show)

type AppResult a = Either AppErr a

safeHead :: [a] -> AppResult a
safeHead [] = Left AppErr
safeHead (x : _) = Right x

-- >>> safeHead ([1, 2, 3] :: [Int])
-- Right 1
-- >>> safeHead ([] :: [Int])
-- Left AppErr

-- 3.7.2

newtype NewTypeIndexed a = NewTypeIndexed {unNewTypeIndexed :: (Integer, a)} deriving (Show)

x :: NewTypeIndexed String
x = NewTypeIndexed (10, "ten")

y :: NewTypeIndexed String
y = NewTypeIndexed (12, "twelve")

-- >>> NewTypeIndexed $ unNewTypeIndexed x
-- NewTypeIndexed {unNewTypeIndexed = (10,"ten")}
-- >>> unNewTypeIndexed $ NewTypeIndexed (2, "two")
-- (2,"two")
-- >>> snd $ unNewTypeIndexed y
-- "twelve"

-- 3.8.3

data Dog = Dog deriving (Show)

data Cat = Cat deriving (Show)

newtype Human = Human String deriving (Show)

class Greeting a where
  name :: a -> String

  hello :: a -> String
  hello _ = "..."

  bye :: a -> String
  bye _ = "..."

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

-- >>> hello Dog
-- "Bark!"
-- >>> hello Cat
-- "..."
-- >>> hello $ Human "takashi"
-- "Hi, I'm takashi."

-- >>> bye Dog
-- "..."
-- >>> bye Cat
-- "Meow..."
-- >>> bye $ Human "takashi"
-- "See you."

-- 3.9.1

sayHello :: Greeting a => a -> IO ()
sayHello x = putStrLn $ hello x

-- 3.9.2
class Greeting a => Laughing a where
  laugh :: a -> String

instance Laughing Human where
  laugh _ = "Zehahahah...!!"

leaveWithLaugh :: Laughing a => a -> IO ()
leaveWithLaugh x = do
  putStrLn $ bye x
  putStrLn $ laugh x

-- 3.9.3

liftGreet :: (a -> String) -> ([a] -> String)
liftGreet f = L.intercalate "\n" . map f

instance Greeting a => Greeting [a] where
  name = liftGreet name
  hello = liftGreet hello
  bye = liftGreet bye

-- 3.9.4

class Breeding a where
  breed :: String -> a

instance Breeding Human where
  breed = Human

clone :: (Breeding a, Greeting a) => a -> a
clone x = breed (name x) `asTypeOf` x

-- >>> hello $ clone $ Human "takashi"
-- "Hi, I'm takashi."

newtype BreedingString = BreedingString {unBreedingString :: String} deriving (Show)

instance Breeding BreedingString where
  breed = BreedingString

-- >>> breed "a raw string" :: BreedingString
-- BreedingString {unBreedingString = "a raw string"}
