module Main06 where

import System.Directory
  ( Permissions (readable, writable),
    findFile,
    findFiles,
    findFilesWith,
    getCurrentDirectory,
    getPermissions,
    setPermissions,
  )

-- 4.4.2

main :: IO ()
main = getPermissions "sample.txt" >>= setPermissions "sample.txt" . toReadAndWritable

toReadAndWritable :: Permissions -> Permissions
toReadAndWritable p = p {readable = True, writable = True}

main' :: IO ()
main' = do
  current <- getCurrentDirectory
  findFiles [current ++ "/..", current] "target.txt" >>= print
  findFile [current ++ "/..", current] "target.txt" >>= print

main'' :: IO ()
main'' = do
  current <- getCurrentDirectory
  let checkWritable filePath = getPermissions filePath >>= pure . writable
  findFilesWith checkWritable [current ++ "/..", current] "target.txt" >>= print
