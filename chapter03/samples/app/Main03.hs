module Main03 where

-- 3.4.4

data LazyAndStrict = LazyAndStrict
  { lsLazy :: Int,
    lsStrict :: !Int
  }

main :: IO ()
main = do
  print $ lsStrict $ LazyAndStrict undefined 2
