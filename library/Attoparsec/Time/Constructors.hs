-- |
-- Efficient construction functions for values of the \"time\" library.
module Attoparsec.Time.Constructors where

import Attoparsec.Time.Prelude


{-# INLINE timeZone #-}
timeZone :: Bool -> Int -> Int -> TimeZone
timeZone positive hour minute =
  minutesToTimeZone $
  bool negate id positive $
  hour * 60 + minute

{-# INLINE day #-}
day :: Int -> Int -> Int -> Day
day y m d =
  fromGregorian (fromIntegral y) m d

{-# INLINE timeOfDay #-}
timeOfDay :: Int -> Int -> Pico -> TimeOfDay
timeOfDay =
  TimeOfDay

{-# INLINE zonedTime #-}
zonedTime :: Day -> TimeOfDay -> TimeZone -> ZonedTime
zonedTime day tod tz =
  ZonedTime (LocalTime day tod) tz

{-# INLINE utcTimeFromDayAndTimeOfDay #-}
utcTimeFromDayAndTimeOfDay :: Day -> TimeOfDay -> TimeZone -> UTCTime
utcTimeFromDayAndTimeOfDay day tod tz =
  zonedTimeToUTC (zonedTime day tod tz)

{-# INLINE utcTimeFromComponents #-}
utcTimeFromComponents :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> TimeZone -> UTCTime
utcTimeFromComponents year month day hour minute second millisecond timeZone =
  undefined
