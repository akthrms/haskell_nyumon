module Main05 where

import Data.Bits (Bits (complement))
import qualified Data.ByteString as B

-- 4.3.5

main :: IO ()
main = do
  s <- B.readFile "sample"
  B.writeFile "sample" . B.map complement $ s
