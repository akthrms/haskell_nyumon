{-# LANGUAGE TemplateHaskell #-}

module Chapter0708 where

import Control.Lens
import Control.Monad.State

-- 7.8.1

value :: (Integer, (Integer, Integer, (Integer, Integer, Integer)), Integer)
value = (1, (2, 3, (999, 4, 5)), 6)

-- >>> value ^. _2 . _3 . _1
-- 999

-- 7.8.2

-- >>> _2 . _3 . _1 .~ "New Value" $ value
-- (1,(2,3,("New Value",4,5)),6)
-- >>> value & _2 . _3 . _1 .~ "New Value"
-- (1,(2,3,("New Value",4,5)),6)

-- 7.8.3

data User = User {_userName :: String, _userAge :: Int, userPassword :: String} deriving (Show)

makeLenses ''User

user :: User
user = User {_userName = "Taro", _userAge = 25, userPassword = "12345"}

-- >>> user ^. userName
-- "Taro"
-- >>> user ^. userAge
-- 25
-- >>> user & userName .~ "Jiro"
-- User {_userName = "Jiro", _userAge = 25, userPassword = "12345"}

userPasswordLens :: Lens User User String String
userPasswordLens = lens userPassword $ \u p -> u {userPassword = p}

-- >>> user ^. userPasswordLens
-- "12345"
-- >>> user & userPasswordLens .~ "new-password"
-- User {_userName = "Taro", _userAge = 25, userPassword = "new-password"}

lensWithState :: State User Int
lensWithState = do
  age <- use userAge
  userName .= "Jiro"
  pure age

-- >>> runState lensWithState user
-- (25,User {_userName = "Jiro", _userAge = 25, userPassword = "12345"})

leftValue :: Either String String
leftValue = Left "Left Value"

-- >>> leftValue & _Left .~ "New Value"
-- Left "New Value"
-- >>> leftValue & _Right .~ "New Value"
-- Left "Left Value"
-- >>> leftValue ^? _Left
-- Just "Left Value"
-- >>> leftValue ^? _Right
-- Nothing
