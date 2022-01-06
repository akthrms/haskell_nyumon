module Main12 where

import Pipes (Pipe, runEffect, (>->))
import qualified Pipes.Prelude as P

-- 7.10.2

hello :: Pipe String String IO r
hello = do
  P.mapM $ \s -> do
    putStrLn ("input: " ++ s)
    pure ("Hello, " ++ s)

main :: IO ()
main = runEffect (P.stdinLn >-> hello >-> P.stdoutLn)
