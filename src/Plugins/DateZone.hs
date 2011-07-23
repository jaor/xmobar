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
-- A date plugin with localization support for Xmobar
--
-- Based on Plugins.Date
--
-- Usage example: in template put
--
-- > Run DateZone "%H:%M:%S" "utcDate" "UTC" 10
--
-----------------------------------------------------------------------------

module Plugins.DateZone (DateZone(..)) where

import Plugins

import System.Locale

import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series

data DateZone = DateZone String String String Int
    deriving (Read, Show)

instance Exec DateZone where
    alias (DateZone _ a _ _) = a
    run   (DateZone f _ z _) = date f z
    rate  (DateZone _ _ _ r) = r

date :: String -> String -> IO String
date format zone = do
  timeZone <- getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ zone)
  zonedTime <- getZonedTime
  return $ formatTime defaultTimeLocale format $ utcToLocalTime' timeZone $ zonedTimeToUTC zonedTime
