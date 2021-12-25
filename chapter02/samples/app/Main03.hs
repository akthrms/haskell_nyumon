module Main03 where

-- 2.6.2

heavyPred :: a -> Bool
heavyPred _ = True

f :: a -> Maybe a
f n = if heavyPred n then Just n else Nothing

main :: IO ()
main = do
  putStrLn "start pattern 1"
  print $ case f 1000 of Just n -> n
  putStrLn "end pattern 1"

  putStrLn "start pattern 2"
  print $ case f 1000 of Just _ -> 0
  putStrLn "end pattern 2"

  putStrLn "start pattern 3"
  print $ case f 1000 of ~(Just _) -> 0
  putStrLn "end pattern 3"
