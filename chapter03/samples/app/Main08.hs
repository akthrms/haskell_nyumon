module Main08 where

import Data.List (intercalate)

-- 3.9

data Dog = Dog deriving (Show)

data Cat = Cat deriving (Show)

data Human = Human String deriving (Show)

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
  bye _ = "See you"

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

liftGreet :: (a -> String) -> [a] -> String
liftGreet f = intercalate "\n" . map f

instance Greeting a => Greeting [a] where
  name = liftGreet name
  hello = liftGreet hello
  bye = liftGreet bye

class Breeding a where
  breed :: String -> a

instance Breeding Human where
  breed = Human

clone :: (Breeding a, Greeting a) => a -> a
-- clone = breed . name
clone x = breed (name x) `asTypeOf` x

newtype BString = NewBString {unBString :: String} deriving (Show)

instance Breeding BString where
  breed = NewBString

main :: IO ()
main = do
  sayHello $ Human "takashi"

  leaveWithLaugh $ Human "takashi"

  sayHello [Human "atsuhiko", Human "shingo"]

  let baby = breed "takeshi"
  print $ hello (baby :: Human)

  print $ hello $ clone $ Human "takeshi"

  print (breed "a raw string" :: BString)
