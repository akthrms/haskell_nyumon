module Main04 where

-- 3.5

data Employee = NewEmployee
  { employeeAge :: Integer,
    employeeIsManager :: Bool,
    employeeName :: String
  }
  deriving (Show)

employee :: Employee
employee =
  NewEmployee
    { employeeAge = 29,
      employeeIsManager = False,
      employeeName = "Subhash Khot"
    }

employee' :: Employee
employee' =
  employee
    { employeeAge = employeeAge employee + 1,
      employeeIsManager = True
    }

main :: IO ()
main = do
  print $ employeeAge employee
  print $ employeeIsManager employee
  print $ employeeName employee
