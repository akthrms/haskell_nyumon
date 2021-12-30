module Main07 where

import Control.Monad.Trans.Reader
  ( Reader,
    ask,
    asks,
    local,
    runReader,
  )

-- 5.5.1

readRound :: Reader Double Int
readRound = do
  x <- ask
  pure (round x)

data PowerEnv = PowerEnv
  { powEnergy :: !Double,
    powSaveMode :: !Bool
  }

consume :: Reader PowerEnv Double
consume = do
  energy <- asks powEnergy
  saveMode <- asks powSaveMode
  let consumption = if saveMode then energy / 10.0 else energy
  pure consumption

testrun :: PowerEnv -> Double
testrun env = (`runReader` env) $ do
  cons1 <- consume
  cons2 <- consume
  consOthers <- local (\e -> e {powSaveMode = True}) $ do
    cons3 <- consume
    cons4 <- consume
    pure (cons3 + cons4)
  pure (cons1 + cons2 + consOthers)

main :: IO ()
main = do
  print (runReader readRound 1.5013232)

  print (runReader consume (PowerEnv 10.0 True))

  print (testrun (PowerEnv 100.0 False))
