-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.CatInt
-- Copyright   :  (c) Nathaniel Wesley Filardo
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Nathaniel Wesley Filardo
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Plugins.Monitors.CatInt where

import Plugins.Monitors.Common
import Plugins.Monitors.CoreCommon

catIntConfig :: IO MConfig
catIntConfig = mkMConfig "<v>" ["v"]

runCatInt :: FilePath -> [String] -> Monitor String
runCatInt p _ =
  let failureMessage = "Cannot read: " ++ show p
      fmt x = show (truncate x :: Int)
  in  checkedDataRetrieval failureMessage [[p]] Nothing id fmt
