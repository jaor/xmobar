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

-- |
-- Core temperature default configuration. Default template contains only one
-- core temperature, user should specify custom template in order to get more
-- core frequencies.
coreTempConfig :: IO MConfig
coreTempConfig = mkMConfig
       "Temp: <core0>C" -- template
       (zipWith (++) (repeat "core") (map show [0 :: Int ..])) -- available
                                                               -- replacements

-- |
-- Function retrieves monitor string holding the core temperature
-- (or temperatures)
runCoreTemp :: [String] -> Monitor String
runCoreTemp _ = let path = ["/sys/bus/platform/devices/coretemp.", "/temp", "_input"]
                    lbl  = Just ("_label", read . drop 5)
                    divisor = 1e3 :: Double
                    failureMessage = "CoreTemp: N/A"
                in  checkedDataRetrieval failureMessage path lbl (/divisor) show

