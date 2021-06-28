module Chapter0710 where

import Pipes
import qualified Pipes.Prelude as P

-- 7.10.2

hello :: Pipe String String IO r
hello = do
  P.mapM $ \s -> do
    putStrLn $ "input: " ++ s
    pure $ "Hello, " ++ s

run :: IO ()
run = runEffect $ P.stdinLn >-> hello >-> P.stdoutLn

-- 7.10.3

sample1 :: IO ()
sample1 = runEffect $ sampleProducer >-> P.map ("input: " ++) >-> P.stdoutLn

sampleProducer :: Producer String IO ()
sampleProducer = do
  yield "Hoge"
  yield "Piyo"
  yield "Fuga"

sample2 :: IO ()
sample2 = runEffect $ range 1 20 >-> sampleConsumer

range :: Int -> Int -> Producer Int IO ()
range n m = mapM_ yield [n .. m]

sampleConsumer :: Consumer Int IO ()
sampleConsumer = do
  n <- await
  lift . putStrLn $ fizzBuzz n
  sampleConsumer

fizzBuzz :: Int -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n
