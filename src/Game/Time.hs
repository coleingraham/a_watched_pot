{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Time where

import           Data.Binary
import           Data.Hashable
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import           Game.State

-- |Represents fractional hours since last play.
newtype HoursSince = HoursSince {
        unHoursSince :: Double
    } deriving (Show,Eq,Num,Binary,Hashable)

fractionalHoursSince :: NominalDiffTime -> HoursSince
fractionalHoursSince d
    = HoursSince $ (*24) $ realToFrac d / realToFrac posixDayLength

-- |Updates the 'lastSessionTime' of the provided 'State' and calculates the
-- 'HoursSince' the 'State' was last opened.
timeSinceLastSession :: State -> IO (State,HoursSince)
timeSinceLastSession s = do
    t <- getCurrentTime
    return (s { lastSessionTime = t }
           ,fractionalHoursSince $ diffUTCTime t (lastSessionTime s))

