module Main08 where

import Control.Exception (bracket)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

-- 4.5.2

main :: IO ()
main =
  bracket (openFile "dummyFileName" ReadMode) hClose $ \h -> do
    s <- hGetContents h
    putStrLn s
