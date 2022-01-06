{-# LANGUAGE TemplateHaskell #-}

module Main10 where

import Control.Lens (Lens, lens, makeLenses, (&), (.~), (^.))

data User = User
  { _userName :: String,
    _userAge :: Int,
    userPassword :: String
  }
  deriving (Show)

makeLenses ''User

userPass :: Lens User User String String
userPass = lens userPassword (\user password -> user {userPassword = password})

main :: IO ()
main = do
  let user =
        User
          { _userName = "Taro",
            _userAge = 25,
            userPassword = "12345"
          }

  print (user ^. userName)
  print (user ^. userAge)
  print (user & userName .~ "Jiro")

  print (user ^. userPass)
  print (user & userPass .~ "new-password")
