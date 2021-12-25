module Main02 where

-- 2.5

putW, putX :: IO ()
putW = putStrLn "W"
putX = putStrLn "X"

makePutY, makePutZ :: IO (IO ())
makePutY = pure $ putStrLn "Y"
makePutZ = pure $ putStrLn "Z"

main :: IO ()
main = do
  let w = putW
      x = putX

  w

  putY <- makePutY
  putZ <- makePutZ

  putZ
