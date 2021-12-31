module Main13 where

import Control.Monad.Except
  ( Except,
    MonadError (catchError, throwError),
    runExcept,
  )
import Control.Monad.Trans.Writer (WriterT (runWriterT), tell)

-- 5.8.6

mtlSample :: Either String ((), String)
mtlSample = runExcept $
  runWriterT $ do
    tell "Start\n"

    (`catchError` handler) $ do
      _ <- throwError "some exception"
      tell "Never reach here\n"

    tell "End\n"
  where
    handler :: String -> WriterT String (Except String) ()
    handler e = tell ("Caught the exception: " ++ e ++ "\n")

main :: IO ()
main = print mtlSample
