{-# LANGUAGE DoAndIfThenElse #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.DateZone
-- Copyright   :  (c) Martin Perner
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Martin Perner <martin@perner.cc>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A date plugin with localization and location support for Xmobar
--
-- Based on Plugins.Date
--
-- Usage example: in template put
--
-- > Run DateZone "%a %H:%M:%S" "de_DE.UTF-8" "UTC" "utcDate" 10
--
-----------------------------------------------------------------------------

module Plugins.DateZone (DateZone(..)) where

import Plugins

import Localize

import Control.Concurrent.STM

import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series

import System.IO.Unsafe
import System.Locale (TimeLocale)



{-# NOINLINE localeLock #-}
-- ensures that only one plugin instance sets the locale
localeLock :: TMVar Bool
localeLock = unsafePerformIO (newTMVarIO False)

data DateZone = DateZone String String String String Int
    deriving (Read, Show)

instance Exec DateZone where
    alias (DateZone _ _ _ a _) = a
    start (DateZone f l z _ r) cb = do
      lock <- atomically $ takeTMVar localeLock
      setupTimeLocale l
      locale <- getTimeLocale
      atomically $ putTMVar localeLock lock
      if z /= "" then do
        timeZone <- getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ z)
        go (dateZone f locale timeZone)
       else
        go (date f locale)

      where go func = func >>= cb >> tenthSeconds r >> go func

date :: String -> TimeLocale -> IO String
date format loc = getZonedTime >>= return . formatTime loc format

dateZone :: String -> TimeLocale -> TimeZoneSeries -> IO String
dateZone format loc timeZone = getZonedTime >>= return . formatTime loc format . utcToLocalTime' timeZone . zonedTimeToUTC
--   zonedTime <- getZonedTime
--   return $ formatTime loc format $ utcToLocalTime' timeZone $ zonedTimeToUTC zonedTime
