module Main14 where

import Control.Exception.Lifted (bracket)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

-- 5.8.6

path :: FilePath
path = "sample.txt"

main :: IO ()
main = (`runReaderT` path) $ do
  bracket open close $ \h -> do
    content <- lift (hGetContents h)
    lift (print (length content))
  where
    open = do
      p <- ask
      lift (openFile p ReadMode)
    close = lift . hClose
