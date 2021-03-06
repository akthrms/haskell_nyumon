module Main11 where

import Control.Monad (forM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)

-- 5.8.2

data Env = Env {envCount :: !(IORef Int)}

countup :: Int -> ReaderT Env IO ()
countup n = do
  ref <- asks envCount
  lift (modifyIORef ref (+ n))

count :: ReaderT Env IO Int
count = asks envCount >>= lift . readIORef

sum10 :: ReaderT Env IO ()
sum10 = do
  forM_ [1 .. 10] $ \i -> do
    countup i
    n <- count
    lift (putStrLn ("sum=" ++ show n))

main :: IO ()
main = do
  ref <- newIORef 0
  runReaderT sum10 (Env ref)
  readIORef ref >>= print
