module Chapter03 where

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
--- >>> cmdOptionToInt $ CmdOptionString "0x78"
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
--- >>> lookup' "pinnyu" dict
-- Nothing

lookup' :: Ord k => k -> TreeDict k v -> Maybe v
lookup' _ TreeDictEmpty = Nothing
lookup' k (TreeDictNode k' v' l r)
  | k < k' = lookup' k l
  | k > k' = lookup' k r
  | otherwise = Just v'

--- >>> M.lookup "shu1" dict'
-- Just 0

dict' :: M.Map String Integer
dict' =
  M.insert "hiratara" 39
    . M.insert "shu1" 0
    . M.insert "masahiko" 63
    $ M.empty
