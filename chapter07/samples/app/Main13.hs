module Main13 where

import Pipes
  ( Consumer,
    MonadTrans (lift),
    Producer,
    await,
    runEffect,
    yield,
    (>->),
  )
import qualified Pipes.Prelude as P

-- 7.10.3

sampleProducer :: Producer String IO ()
sampleProducer = do
  yield "Hoge"
  yield "Piyo"
  yield "Fuga"

sample1 :: IO ()
sample1 = runEffect (sampleProducer >-> P.map ("input: " ++) >-> P.stdoutLn)

range :: Int -> Int -> Producer Int IO ()
range n m = mapM_ yield [n .. m]

sampleConsumer :: Consumer Int IO ()
sampleConsumer = do
  x <- await
  lift . putStrLn $ fizzBuzz x
  sampleConsumer

fizzBuzz :: Int -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n

sample2 :: IO ()
sample2 = runEffect (range 1 20 >-> sampleConsumer)

main :: IO ()
main = do
  sample1
  sample2
