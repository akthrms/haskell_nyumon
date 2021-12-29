module Main02 where

-- 5.1.2

type Category = String

type Name = String

type Price = Integer

type Menu = [(Category, [(Name, Price)])]

type Item = (Category, Name, Price)

menu :: Menu
menu =
  [ ( "Foods",
      [ ("Hamburger", 120),
        ("FrenchFries", 100)
      ]
    ),
    ( "Drinks",
      [ ("Cola", 80),
        ("Tea", 100)
      ]
    )
  ]

getItem :: Menu -> Category -> Name -> Maybe Item
getItem menu category name = do
  subMenu <- lookup category menu
  price <- lookup name subMenu
  pure (category, name, price)

main :: IO ()
main = do
  print (getItem menu "Foods" "Hamburger")
