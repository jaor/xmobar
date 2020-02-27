{-# LANGUAGE CPP #-}
#ifdef USE_NL80211
{-# LANGUAGE TypeApplications #-}
#endif
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
-- A monitor reporting SSID and signal level for wireless interfaces
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Wireless (wirelessConfig, runWireless)  where

import System.Console.GetOpt

import Xmobar.Plugins.Monitors.Common

#ifdef IWLIB
import Network.IWlib
#elif defined USE_NL80211
import Control.Exception (bracket)
import qualified Data.Map as M
import GHC.Int (Int8)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.ByteString.Char8 (unpack)
import Data.Serialize.Put (runPut, putWord32host, putByteString)
import Data.Serialize.Get (runGet)

import System.Linux.Netlink hiding (query)
import System.Linux.Netlink.GeNetlink.NL80211
import System.Linux.Netlink.GeNetlink.NL80211.StaInfo
import System.Linux.Netlink.GeNetlink.NL80211.Constants
import System.Posix.IO (closeFd)

data IwData = IwData { wiEssid :: String, wiSignal :: Maybe Int, wiQuality :: Int }

getWirelessInfo :: String -> IO IwData
getWirelessInfo ifname =
  bracket makeNL80211Socket (closeFd . getFd) (\s -> do
  iflist <- getInterfaceList s
  iwdata <- runMaybeT $ do
    ifidx <- MaybeT . return $ foldr (\(n, i) z ->
                                       if ifname == "" || ifname == n then Just i else z)
                                     Nothing
                                     iflist
    scanp <- liftIO (getConnectedWifi s ifidx) >>=
             MaybeT . return . listToMaybe
    bssid <- MaybeT . return $ M.lookup eNL80211_ATTR_BSS (packetAttributes scanp) >>=
                               rightToMaybe . runGet getAttributes >>=
                               M.lookup eNL80211_BSS_BSSID
    stap <- liftIO (query s eNL80211_CMD_GET_STATION True $ M.fromList
                          [(eNL80211_ATTR_IFINDEX, runPut $ putWord32host ifidx),
                           (eNL80211_ATTR_MAC, runPut $ putByteString bssid)]) >>=
            MaybeT . return . listToMaybe
    let ssid   = fromMaybe "" $ getWifiAttributes scanp >>= M.lookup eWLAN_EID_SSID >>=
                                return . unpack
        signal = staInfoFromPacket stap >>= staSignalMBM >>=
                 return . fromIntegral @Int8 . fromIntegral
        qlty   = maybe (-1) (round @Float . (/ 0.7) . (+ 110) .
                                            clamp (-110) (-40) . fromIntegral) signal
    MaybeT . return $ Just $ IwData ssid signal qlty
  return $ fromMaybe (IwData "" Nothing (-1)) iwdata)
  where
    rightToMaybe = either (const Nothing) Just
    clamp lb up v
      | v < lb = lb
      | v > up = up
      | otherwise = v
#endif

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
  mkMConfig "<ssid> <quality>"
            ["ssid", "essid", "signal", "quality", "qualitybar", "qualityvbar", "qualityipat"]

runWireless :: String -> [String] -> Monitor String
runWireless iface args = do
  opts <- io $ parseOptsWith options defaultOpts args
#ifdef IWLIB
  iface' <- if "" == iface then io findInterface else return iface
#else
  let iface' = iface
#endif
  wi <- io $ getWirelessInfo iface'
  na <- getConfigValue naString
  let essid = wiEssid wi
      qlty = fromIntegral $ wiQuality wi
      e = if essid == "" then na else essid
  ep <- showWithPadding e
#ifdef USE_NL80211
  let s = wiSignal wi
#else
  let s = if qlty >= 0 then Just (qlty * 0.7 - 110) else Nothing
#endif
  sp <- showWithPadding $ maybe "" show s
  q <- if qlty >= 0
       then showPercentWithColors (qlty / 100)
       else showWithPadding ""
  qb <- showPercentBar qlty (qlty / 100)
  qvb <- showVerticalBar qlty (qlty / 100)
  qipat <- showIconPattern (qualityIconPattern opts) (qlty / 100)
  parseTemplate [ep, ep, sp, q, qb, qvb, qipat]

#ifdef IWLIB
findInterface :: IO String
findInterface = do
  c <- readFile "/proc/net/wireless"
  let nds = lines c
  return $ if length nds > 2 then takeWhile (/= 'c') (nds!!2) else []
#endif
