{- |
   Module     : Data.MaybeConvertible.Instances.Time
   Copyright  : Copyright (C) 2009-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Instances to convert between various time structures, both old- and new-style.

At present, this module does not do full input validation.  That is, it is possible
to get an exception rather than a Left result from these functions if your input is
invalid, particularly when converting from the old-style System.Time structures.

Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

module Data.Convert.Instances.Time()
where

import Data.Convert.Base
import Data.Convert.Bound
import Data.Convert.Instances.Num()
import qualified System.Time as ST
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Calendar.OrdinalDate
import Data.Ratio
import Foreign.C.Types

------------------------------------------------------------------------
---- Intra-System.Time stuff
------------------------------------------------------------------------

--instance MaybeConvertible ST.ClockTime ST.CalendarTime where
--    safeConvert = return . ST.toUTCTime

--instance MaybeConvertible ST.CalendarTime ST.ClockTime where
--    safeConvert = return . ST.toClockTime

--instance MaybeConvertible ST.ClockTime Integer where
--    safeConvert (ST.TOD x _) = return x

--instance MaybeConvertible Integer ST.ClockTime where
--    safeConvert x = return $ ST.TOD x 0

------------------------------------------------------------------------
---- Intra-Data.Time stuff
------------------------------------------------------------------------

-------------------------------- POSIX and UTC times
--{- Covered under Real a
--instance MaybeConvertible Rational POSIXTime where
--    safeConvert = return . fromRational
---}

--instance MaybeConvertible Rational POSIXTime where
--    safeConvert = return . fromRational
--instance MaybeConvertible Integer POSIXTime where
--    safeConvert = return . fromInteger
--instance MaybeConvertible Int POSIXTime where
--    safeConvert = return . fromIntegral
--instance MaybeConvertible Double POSIXTime where
--    safeConvert = return . realToFrac

--instance MaybeConvertible POSIXTime Integer where
--    safeConvert = return . truncate
--instance MaybeConvertible POSIXTime Rational where
--    safeConvert = return . toRational
--instance MaybeConvertible POSIXTime Double where
--    safeConvert = return . realToFrac
--instance MaybeConvertible POSIXTime Int where
--    safeConvert = boundedConversion (return . truncate)

--instance MaybeConvertible POSIXTime UTCTime where
--    safeConvert = return . posixSecondsToUTCTime
--instance MaybeConvertible UTCTime POSIXTime where
--    safeConvert = return . utcTimeToPOSIXSeconds

--instance MaybeConvertible Rational UTCTime where
--    safeConvert a = safeConvert a >>= return . posixSecondsToUTCTime
--instance MaybeConvertible Integer UTCTime where
--    safeConvert a = safeConvert a >>= return . posixSecondsToUTCTime
--instance MaybeConvertible Int UTCTime where
--    safeConvert a = safeConvert a >>= return . posixSecondsToUTCTime
--instance MaybeConvertible Double UTCTime where
--    safeConvert a = safeConvert a >>= return . posixSecondsToUTCTime

--instance MaybeConvertible UTCTime Rational where
--    safeConvert = safeConvert . utcTimeToPOSIXSeconds
--instance MaybeConvertible UTCTime Integer where
--    safeConvert = safeConvert . utcTimeToPOSIXSeconds
--instance MaybeConvertible UTCTime Double where
--    safeConvert = safeConvert . utcTimeToPOSIXSeconds
--instance MaybeConvertible UTCTime Int where
--    safeConvert = boundedConversion (safeConvert . utcTimeToPOSIXSeconds)

-------------------------------- LocalTime stuff

--instance MaybeConvertible UTCTime ZonedTime where
--    safeConvert = return . utcToZonedTime utc
--instance MaybeConvertible POSIXTime ZonedTime where
--    safeConvert = return . utcToZonedTime utc . posixSecondsToUTCTime
--instance MaybeConvertible ZonedTime UTCTime where
--    safeConvert = return . zonedTimeToUTC
--instance MaybeConvertible ZonedTime POSIXTime where
--    safeConvert = return . utcTimeToPOSIXSeconds . zonedTimeToUTC

--{- Too obvious?
--instance MaybeConvertible LocalTime Day where
--    safeConvert = return . localDay
--instance MaybeConvertible LocalTime TimeOfDay where
--    safeConvert = return . localTimeOfDay
---}

------------------------------------------------------------------------
---- Conversions between old and new time
------------------------------------------------------------------------
--instance MaybeConvertible ST.CalendarTime ZonedTime where
--    safeConvert ct = return $ ZonedTime {
--     zonedTimeToLocalTime = LocalTime {
--       localDay = fromGregorian (fromIntegral $ ST.ctYear ct)
--                  (1 + (fromEnum $ ST.ctMonth ct))
--                  (ST.ctDay ct),
--       localTimeOfDay = TimeOfDay {
--         todHour = ST.ctHour ct,
--         todMin = ST.ctMin ct,
--         todSec = (fromIntegral $ ST.ctSec ct) +
--                  fromRational (ST.ctPicosec ct % 1000000000000)
--                        }
--                            },
--     zonedTimeZone = TimeZone {
--                       timeZoneMinutes = ST.ctTZ ct `div` 60,
--                       timeZoneSummerOnly = ST.ctIsDST ct,
--                       timeZoneName = ST.ctTZName ct}
--}

--instance MaybeConvertible ST.CalendarTime POSIXTime where
--    safeConvert = convertVia (undefined::ST.ClockTime)
--instance MaybeConvertible ST.CalendarTime UTCTime where
--    safeConvert = convertVia (undefined::POSIXTime)

--instance MaybeConvertible ST.ClockTime POSIXTime where
--    safeConvert (ST.TOD x y) = return $ fromRational $
--                                        fromInteger x + fromRational (y % 1000000000000)
--instance MaybeConvertible ST.ClockTime UTCTime where
--    safeConvert = convertVia (undefined::POSIXTime)
--instance MaybeConvertible ST.ClockTime ZonedTime where
--    safeConvert = convertVia (undefined::UTCTime)
--instance MaybeConvertible ZonedTime ST.ClockTime where
--    safeConvert = convertVia (undefined::POSIXTime)

--instance MaybeConvertible POSIXTime ST.ClockTime where
--    safeConvert x = return $ ST.TOD rsecs rpico
--        where rsecs = floor x
--              rpico = truncate $ abs $ 1000000000000 * (x - (fromIntegral rsecs))
--instance MaybeConvertible UTCTime ST.ClockTime where
--    safeConvert = safeConvert . utcTimeToPOSIXSeconds

--instance MaybeConvertible ZonedTime ST.CalendarTime where
--    safeConvert zt = return $ ST.CalendarTime {
--            ST.ctYear = fromIntegral year,
--            ST.ctMonth = toEnum (month - 1),
--            ST.ctDay = day,
--            ST.ctHour = todHour ltod,
--            ST.ctMin = todMin ltod,
--            ST.ctSec = secs,
--            ST.ctPicosec = pico,
--            ST.ctWDay = toEnum . snd . sundayStartWeek . localDay . zonedTimeToLocalTime $ zt,
--            ST.ctYDay = (snd . toOrdinalDate . localDay . zonedTimeToLocalTime $ zt) - 1,
--            ST.ctTZName = timeZoneName . zonedTimeZone $ zt,
--            ST.ctTZ = (timeZoneMinutes . zonedTimeZone $ zt) * 60,
--            ST.ctIsDST = timeZoneSummerOnly . zonedTimeZone $ zt
--          }
--        where (year, month, day) = toGregorian . localDay . zonedTimeToLocalTime $ zt
--              ltod = localTimeOfDay . zonedTimeToLocalTime $ zt
--              secs = (truncate . todSec $ ltod)::Int
--              picoRational = toRational (todSec ltod) - toRational secs
--              pico = truncate (picoRational * 1000000000000)
--instance MaybeConvertible POSIXTime ST.CalendarTime where
--    safeConvert = convertVia (undefined::ZonedTime)
--instance MaybeConvertible UTCTime ST.CalendarTime where
--    safeConvert = safeConvert . utcTimeToPOSIXSeconds

--instance MaybeConvertible ST.TimeDiff NominalDiffTime where
--    {- This is a clever hack.  We convert the TimeDiff to a ClockTime, applying
--       it as a diff vs. the epoch.  Converting this ClockTime to a POSIXTime yiels
--       the NominalDiffTime we want, since a POSIXTime is a NominalDiffTime vs. the
--       epoch. -}
--    safeConvert td = safeConvert clockTime
--        where clockTime = ST.addToClockTime td (ST.TOD 0 0)
--instance MaybeConvertible NominalDiffTime ST.TimeDiff where
--    {- Similar clever hack as above. -}
--    safeConvert ndt =
--        do clockt <- safeConvert ndt
--           return (ST.diffClockTimes clockt (ST.TOD 0 0))

--instance MaybeConvertible Integer ST.TimeDiff where
--    safeConvert = convertVia (undefined::NominalDiffTime)
--instance MaybeConvertible Double ST.TimeDiff where
--    safeConvert = convertVia (undefined::NominalDiffTime)
--instance MaybeConvertible ST.TimeDiff Integer where
--    safeConvert = convertVia (undefined :: NominalDiffTime)
--instance MaybeConvertible ST.TimeDiff Rational where
--    safeConvert = convertVia (undefined :: NominalDiffTime)
--instance MaybeConvertible ST.TimeDiff Double where
--    safeConvert = convertVia (undefined :: NominalDiffTime)

------------------------------------------------------------------------
---- Foreign.C Types
------------------------------------------------------------------------

--instance MaybeConvertible CTime POSIXTime where
--    safeConvert = return . realToFrac
--instance MaybeConvertible POSIXTime CTime where
--    safeConvert = return . fromInteger . truncate

--instance MaybeConvertible CTime Integer where
--    safeConvert = return . truncate . toRational
--instance MaybeConvertible Integer CTime where
--    safeConvert = return . fromInteger

--instance MaybeConvertible CTime Double where
--    safeConvert = return . realToFrac
--instance MaybeConvertible Double CTime where
--    safeConvert = return . fromInteger . truncate

--instance MaybeConvertible CTime Int where
--    safeConvert x = do r1 <- safeConvert x
--                       boundedConversion (return . fromInteger) r1
--instance MaybeConvertible Int CTime where
--    safeConvert = safeConvert . toInteger

--instance MaybeConvertible CTime UTCTime where
--    safeConvert = convertVia (undefined :: POSIXTime)
--instance MaybeConvertible UTCTime CTime where
--    safeConvert = convertVia (undefined :: POSIXTime)

--instance MaybeConvertible CTime ST.ClockTime where
--    safeConvert = convertVia (undefined :: POSIXTime)
--instance MaybeConvertible ST.ClockTime CTime where
--    safeConvert = convertVia (undefined :: POSIXTime)

--instance MaybeConvertible CTime ST.CalendarTime where
--    safeConvert = convertVia (undefined::POSIXTime)
--instance MaybeConvertible ST.CalendarTime CTime where
--    safeConvert = convertVia (undefined::POSIXTime)

--instance MaybeConvertible CTime ZonedTime where
--    safeConvert = convertVia (undefined::POSIXTime)
--instance MaybeConvertible ZonedTime CTime where
--    safeConvert = convertVia (undefined::POSIXTime)
