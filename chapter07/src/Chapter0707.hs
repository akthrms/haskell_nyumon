module Chapter0707 where

import Data.Time

-- 7.7.1

-- >>> getZonedTime
-- 2021-06-28 00:25:47.024429 JST

-- 7.7.2

add2DaysFromNow :: IO ()
add2DaysFromNow = do
  utcTime@(UTCTime day diffTime) <- getCurrentTime
  print utcTime
  print $ UTCTime (addDays 2 day) diffTime

add1HourFromNow :: IO ()
add1HourFromNow = do
  utcTime <- getCurrentTime
  print utcTime
  print $ addUTCTime (60 * 60) utcTime
