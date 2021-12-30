module Main10 where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)

-- 5.8.2

data Env = Env
  { envX :: !Integer,
    envY :: !Integer
  }

sumEnv :: ReaderT Env IO Integer
sumEnv = do
  x <- asks envX
  y <- asks envY
  pure (x + y)

sumEnvIO :: ReaderT Env IO Integer
sumEnvIO = do
  x <- asks envX
  lift (putStrLn ("x=" ++ show x))
  y <- asks envY
  lift (putStrLn ("y=" ++ show y))
  pure (x + y)

main :: IO ()
main = do
  runReaderT sumEnv (Env 10 20) >>= print
  runReaderT sumEnvIO (Env 10 20) >>= print
