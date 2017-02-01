module Attoparsec.Time
(
  timeOfDayInISO8601,
  dayInISO8601,
  timeZoneInISO8601,
  utcTimeInISO8601,
)
where

import Attoparsec.Time.Prelude hiding (take, takeWhile)
import Data.Attoparsec.Text
import qualified Attoparsec.Time.Constructors as A
import qualified Attoparsec.Time.Validators as B
import qualified Data.Text as C


validated :: Show a => B.Validator a -> Parser a -> Parser a
validated validator parser =
  parser >>= \x -> B.run validator (pure x) fail x

sign :: Parser Bool
sign =
  anyChar >>= \case
    '+' -> return True
    '-' -> return False
    _ -> empty

decimalChar :: Parser Int
decimalChar =
  satisfyWith ((subtract 48) . ord) ((&&) <$> (>= 0) <*> (<= 9))

decimalOfLength :: Num a => Int -> Parser a
decimalOfLength length =
  foldl' (\a b -> a * 10 + b) 0 <$>
  replicateM length (fmap fromIntegral decimalChar) <|>
  fail "Invalid decimal length"

shortMonth :: Parser Int
shortMonth =
  liftM C.toLower (take 3) >>= \case
    "jan" -> return 1
    "feb" -> return 2
    "mar" -> return 3
    "apr" -> return 4
    "may" -> return 5
    "jun" -> return 6
    "jul" -> return 7
    "aug" -> return 8
    "sep" -> return 9
    "oct" -> return 10
    "nov" -> return 11
    "dec" -> return 12
    _ -> empty

picoWithBasisOfLength :: Int -> Parser Pico
picoWithBasisOfLength basisLength =
  (\a b -> MkFixed (foldl' (\a b -> a * 10 + fromIntegral b) 0 (a ++ b))) <$> beforePoint <*> afterPoint
  where
    resolution =
      12
    beforePoint =
      replicateM basisLength decimalChar
    afterPoint =
      padListFromRight 0 [] resolution <$> ((char '.' *> many decimalChar) <|> pure [])
      where
        padListFromRight padding accumulator length list =
          case length of
            0 -> reverse accumulator
            _ -> case list of
              head : tail -> padListFromRight padding (head : accumulator) (pred length) tail
              _ -> reverse accumulator ++ replicate length padding

{-# INLINE hour #-}
hour :: Parser Int
hour =
  validated B.hour (decimalOfLength 2) <?> "hour"

{-# INLINE minute #-}
minute :: Parser Int
minute =
  validated B.minute (decimalOfLength 2) <?> "minute"

{-# INLINE second #-}
second :: Parser Pico
second =
  validated B.second (picoWithBasisOfLength 2) <?> "second"

{-|
>>> parseOnly timeOfDayInISO8601 "05:03:58"
Right 05:03:58

>>> parseOnly timeOfDayInISO8601 "05:03:58.02"
Right 05:03:58.02

>>> parseOnly timeOfDayInISO8601 "05:03:58.020"
Right 05:03:58.02

Checks the elements to be within a proper range:

>>> parseOnly timeOfDayInISO8601 "24:00:00"
Left "timeOfDayInISO8601 > hour: Failed reading: Validator \"hour\" failed on the following input: 24"

>>> parseOnly timeOfDayInISO8601 "00:00:60"
Left "timeOfDayInISO8601 > second: Failed reading: Validator \"second\" failed on the following input: 60.000000000000"

Checks the elements to be of proper length:

>>> parseOnly timeOfDayInISO8601 "1:00:00"
Left "timeOfDayInISO8601 > hour: Failed reading: Invalid decimal length"

>>> parseOnly timeOfDayInISO8601 "01:1:00"
Left "timeOfDayInISO8601 > minute: Failed reading: Invalid decimal length"
-}
{-# INLINE timeOfDayInISO8601 #-}
timeOfDayInISO8601 :: Parser TimeOfDay
timeOfDayInISO8601 =
  unnamedParser <?> "timeOfDayInISO8601"
  where
    unnamedParser =
      A.timeOfDay <$>
      (hour <* char ':') <*>
      (minute <* char ':') <*>
      (second)

{-|
>>> parseOnly dayInISO8601 "2017-02-01"
Right 2017-02-01

Checks the elements to be in proper range:

>>> parseOnly dayInISO8601 "2017-13-01"
Left "dayInISO8601: Failed reading: Invalid combination of year month and day: (2017,13,1)"

That is accounting for leap year:

>>> parseOnly dayInISO8601 "2017-02-29"
Left "dayInISO8601: Failed reading: Invalid combination of year month and day: (2017,2,29)"

>>> parseOnly dayInISO8601 "2016-02-29"
Right 2016-02-29
-}
{-# INLINE dayInISO8601 #-}
dayInISO8601 :: Parser Day
dayInISO8601 =
  unnamedParser <?> "dayInISO8601"
  where
    unnamedParser =
      do
        year <- decimalOfLength 4
        char '-'
        month <- decimalOfLength 2
        char '-'
        day <- decimalOfLength 2
        case fromGregorianValid year month day of
          Just day -> return day
          Nothing -> fail (error year month day)
      where
        error year month day =
          showString "Invalid combination of year month and day: " $ 
          show (year, month, day)

{-|
>>> parseOnly timeZoneInISO8601 "+01:00"
Right +0100

>>> parseOnly timeZoneInISO8601 "+0100"
Right +0100

>>> parseOnly timeZoneInISO8601 "-0100"
Right -0100

>>> parseOnly timeZoneInISO8601 "Z"
Right UTC
-}
timeZoneInISO8601 :: Parser TimeZone
timeZoneInISO8601 =
  unnamedParser <?> "timeZoneInISO8601"
  where
    unnamedParser =
      z <|> offset
      where
        z =
          char 'Z' $> utc
        offset =
          A.timeZone <$> sign <*> decimalOfLength 2 <*> (char ':' *> decimalOfLength 2 <|> decimalOfLength 2 <|> pure 0)

{-|
>>> parseOnly utcTimeInISO8601 "2017-02-01T05:03:58+01:00"
Right 2017-02-01 04:03:58 UTC
-}
utcTimeInISO8601 :: Parser UTCTime
utcTimeInISO8601 =
  unnamedParser <?> "utcTimeInISO8601"
  where
    unnamedParser =
      do
        day <- dayInISO8601
        char 'T'
        time <- timeOfDayInISO8601
        zone <- timeZoneInISO8601
        return (A.utcTimeFromDayAndTimeOfDay day time zone)
