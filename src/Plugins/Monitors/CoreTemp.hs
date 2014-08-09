-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.CoreTemp
-- Copyright   :  (c) Juraj Hercek
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Juraj Hercek <juhe_haskell@hck.sk>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A core temperature monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.CoreTemp where

import Plugins.Monitors.Common
import Plugins.Monitors.CoreCommon


import Data.Char (isDigit)

-- |
-- Core temperature default configuration. Default template contains only one
-- core temperature, user should specify custom template in order to get more
-- core frequencies.
coreTempConfig :: IO MConfig
coreTempConfig = mkMConfig
       "Temp: <core0>C" -- template
       (map ((++) "core" . show) [0 :: Int ..]) -- available
                                                -- replacements

-- |
-- Function retrieves monitor string holding the core temperature
-- (or temperatures)
runCoreTemp :: [String] -> Monitor String
runCoreTemp _ = do
   dn <- getConfigValue decDigits
   failureMessage <- getConfigValue naString
   let path = ["/sys/bus/platform/devices/coretemp.", "/temp", "_input"]
       path' = ["/sys/bus/platform/devices/coretemp.", "/hwmon/hwmon", "/temp", "_input"]
       lbl  = Just ("_label", read . dropWhile (not . isDigit))
       divisor = 1e3 :: Double
       show' = showDigits (max 0 dn)
   checkedDataRetrieval failureMessage [path, path'] lbl (/divisor) show'
