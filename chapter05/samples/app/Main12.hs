module Main12 where

import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.State (evalStateT, get, modify)

-- 5.8.3

main :: IO ()
main = do
  result <- (`evalStateT` 0) $ runExceptT loop
  case result of
    Right _ -> pure ()
    Left e -> putStrLn e
  where
    loop = do
      i <- st get
      unless (i < 3) (throwE "Too much failure")

      op <- io getLine
      if op == "end"
        then pure ()
        else do
          st (modify (+ 1))
          loop

    io = lift . lift
    st = lift
