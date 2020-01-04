-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Wireless
-- Copyright   :  (c) Jose Antonio Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose Antonio Ortega Ruiz
-- Stability   :  unstable
-- Portability :  unportable
--
-- A monitor reporting ESSID and link quality for wireless interfaces
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Wireless (wirelessConfig, runWireless)  where

import System.Console.GetOpt

import Xmobar.Plugins.Monitors.Common
import Network.IWlib

newtype WirelessOpts = WirelessOpts
  { qualityIconPattern :: Maybe IconPattern
  }

defaultOpts :: WirelessOpts
defaultOpts = WirelessOpts
  { qualityIconPattern = Nothing
  }

options :: [OptDescr (WirelessOpts -> WirelessOpts)]
options =
  [ Option "" ["quality-icon-pattern"] (ReqArg (\d opts ->
     opts { qualityIconPattern = Just $ parseIconPattern d }) "") ""
  ]

wirelessConfig :: IO MConfig
wirelessConfig =
  mkMConfig "<essid> <quality>"
            ["essid", "quality", "qualitybar", "qualityvbar", "qualityipat"]

runWireless :: String -> [String] -> Monitor String
runWireless iface args = do
  opts <- io $ parseOptsWith options defaultOpts args
  iface' <- if "" == iface then io findInterface else return iface
  wi <- io $ getWirelessInfo iface'
  na <- getConfigValue naString
  let essid = wiEssid wi
      qlty = fromIntegral $ wiQuality wi
      e = if essid == "" then na else essid
  ep <- showWithPadding e
  q <- if qlty >= 0
       then showPercentWithColors (qlty / 100)
       else showWithPadding ""
  qb <- showPercentBar qlty (qlty / 100)
  qvb <- showVerticalBar qlty (qlty / 100)
  qipat <- showIconPattern (qualityIconPattern opts) (qlty / 100)
  parseTemplate [ep, q, qb, qvb, qipat]

findInterface :: IO String
findInterface = do
  c <- readFile "/proc/net/wireless"
  let nds = lines c
  return $ if length nds > 2 then takeWhile (/= 'c') (nds!!2) else []
