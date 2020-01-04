-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Volume
-- Copyright   :  (c) 2011, 2013, 2015, 2018 Thomas Tuegel
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A monitor for ALSA soundcards
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Volume
  ( runVolume
  , runVolumeWith
  , volumeConfig
  , options
  , defaultOpts
  , VolumeOpts
  ) where

import Control.Applicative ( (<$>), liftA3 )
import Control.Monad ( liftM2, liftM3, mplus )
import Data.Traversable (sequenceA)
import Xmobar.Plugins.Monitors.Common
import Sound.ALSA.Mixer
import qualified Sound.ALSA.Exception as AE
import System.Console.GetOpt

volumeConfig :: IO MConfig
volumeConfig = mkMConfig "Vol: <volume>% <status>"
                         ["volume", "volumebar", "volumevbar", "dB","status", "volumeipat"]


data VolumeOpts = VolumeOpts
    { onString :: String
    , offString :: String
    , onColor :: Maybe String
    , offColor :: Maybe String
    , highDbThresh :: Float
    , lowDbThresh :: Float
    , volumeIconPattern :: Maybe IconPattern
    , lowVolThresh :: Maybe Float
    , highVolThresh :: Maybe Float
    , lowString :: String
    , mediumString :: String
    , highString :: String
    }

defaultOpts :: VolumeOpts
defaultOpts = VolumeOpts
    { onString = "[on] "
    , offString = "[off]"
    , onColor = Just "green"
    , offColor = Just "red"
    , highDbThresh = -5.0
    , lowDbThresh = -30.0
    , volumeIconPattern = Nothing
    , lowVolThresh = Just 20.0
    , highVolThresh = Just 60.0
    , lowString = ""
    , mediumString = ""
    , highString = ""
    }

data VolumeStatus
    = VolLow
    | VolMedium
    | VolHigh
    | VolOff

-- | Set the volume status according to user set thresholds and the current
-- volume
getVolStatus :: Float -- ^ Low volume threshold, in [0,100]
             -> Float -- ^ High volume threshold, in  [0,100]
             -> Float -- ^ Current volume, in [0,1]
             -> VolumeStatus
getVolStatus lo hi val'
    | val >= hi = VolHigh
    | val >= lo = VolMedium
    | otherwise = VolLow
  where
    val = val' * 100

options :: [OptDescr (VolumeOpts -> VolumeOpts)]
options =
    [ Option "O" ["on"] (ReqArg (\x o -> o { onString = x }) "") ""
    , Option "o" ["off"] (ReqArg (\x o -> o { offString = x }) "") ""
    , Option "" ["lowd"] (ReqArg (\x o -> o { lowDbThresh = read x }) "") ""
    , Option "" ["highd"] (ReqArg (\x o -> o { highDbThresh = read x }) "") ""
    , Option "C" ["onc"] (ReqArg (\x o -> o { onColor = Just x }) "") ""
    , Option "c" ["offc"] (ReqArg (\x o -> o { offColor = Just x }) "") ""
    , Option "" ["volume-icon-pattern"] (ReqArg (\x o ->
       o { volumeIconPattern = Just $ parseIconPattern x }) "") ""
    , Option "L" ["lowv"] (ReqArg (\x o -> o { lowVolThresh = Just $ read x }) "") ""
    , Option "H" ["highv"] (ReqArg (\x o -> o { highVolThresh = Just $ read x }) "") ""
    , Option "l" ["lows"] (ReqArg (\x o -> o { lowString = x }) "") ""
    , Option "m" ["mediums"] (ReqArg (\x o -> o { mediumString = x }) "") ""
    , Option "h" ["highs"] (ReqArg (\x o -> o { highString = x }) "") ""
    ]

percent :: Integer -> Integer -> Integer -> Float
percent v' lo' hi' = (v - lo) / (hi - lo)
  where v = fromIntegral v'
        lo = fromIntegral lo'
        hi = fromIntegral hi'

formatVol :: Integer -> Integer -> Integer -> Monitor String
formatVol lo hi v =
    showPercentWithColors $ percent v lo hi

formatVolBar :: Integer -> Integer -> Integer -> Monitor String
formatVolBar lo hi v =
    showPercentBar (100 * x) x where x = percent v lo hi

formatVolVBar :: Integer -> Integer -> Integer -> Monitor String
formatVolVBar lo hi v =
    showVerticalBar (100 * x) x where x = percent v lo hi

formatVolDStr :: Maybe IconPattern -> Integer -> Integer -> Integer -> Monitor String
formatVolDStr ipat lo hi v =
    showIconPattern ipat $ percent v lo hi

switchHelper :: VolumeOpts
             -> (VolumeOpts -> Maybe String)
             -> (VolumeOpts -> String)
             -> VolumeStatus
             -> Monitor String
switchHelper opts cHelp strHelp vs = return $
    colorHelper (cHelp opts)
    ++ volHelper vs opts
    ++ strHelp opts
    ++ maybe "" (const "</fc>") (cHelp opts)

formatSwitch :: VolumeOpts -> Bool -> VolumeStatus -> Monitor String
formatSwitch opts True vs = switchHelper opts onColor onString vs
formatSwitch opts False _  = switchHelper opts offColor offString VolOff

-- | Convert the current volume status into user defined strings
volHelper :: VolumeStatus -> VolumeOpts -> String
volHelper volStatus opts =
    case volStatus of
        VolHigh -> highString opts
        VolMedium -> mediumString opts
        VolLow -> lowString opts
        VolOff -> ""

colorHelper :: Maybe String -> String
colorHelper = maybe "" (\c -> "<fc=" ++ c ++ ">")

formatDb :: VolumeOpts -> Integer -> Monitor String
formatDb opts dbi = do
    h <- getConfigValue highColor
    m <- getConfigValue normalColor
    l <- getConfigValue lowColor
    d <- getConfigValue decDigits
    let db = fromIntegral dbi / 100.0
        digits = showDigits d db
        startColor | db >= highDbThresh opts = colorHelper h
                   | db < lowDbThresh opts = colorHelper l
                   | otherwise = colorHelper m
        stopColor | null startColor = ""
                  | otherwise = "</fc>"
    return $ startColor ++ digits ++ stopColor

runVolume :: String -> String -> [String] -> Monitor String
runVolume mixerName controlName argv = do
    opts <- io $ parseOptsWith options defaultOpts argv
    runVolumeWith opts mixerName controlName

runVolumeWith :: VolumeOpts -> String -> String -> Monitor String
runVolumeWith opts mixerName controlName = do
    (lo, hi, val, db, sw) <- io readMixer
    p <- liftMonitor $ liftM3 formatVol lo hi val
    b <- liftMonitor $ liftM3 formatVolBar lo hi val
    v <- liftMonitor $ liftM3 formatVolVBar lo hi val
    d <- getFormatDB opts db
    let volStat = liftA3 getVolStatus
                         (lowVolThresh opts)
                         (highVolThresh opts)
                         (liftA3 percent val lo hi) -- current volume in %
    s <- getFormatSwitch opts sw volStat
    ipat <- liftMonitor $ liftM3 (formatVolDStr $ volumeIconPattern opts) lo hi val
    parseTemplate [p, b, v, d, s, ipat]

  where

    readMixer =
      AE.catch (withMixer mixerName $ \mixer -> do
                   control <- getControlByName mixer controlName
                   (lo, hi) <- liftMaybe $ getRange <$> volumeControl control
                   val <- getVal $ volumeControl control
                   db <- getDB $ volumeControl control
                   sw <- getSw $ switchControl control
                   return (fmap toInteger lo, fmap toInteger hi, val, db, sw))
                (const $ return (Nothing, Nothing, Nothing, Nothing, Nothing))

    volumeControl :: Maybe Control -> Maybe Volume
    volumeControl c = (playback . volume =<< c)
              `mplus` (capture . volume =<< c)
              `mplus` (common . volume =<< c)

    switchControl :: Maybe Control -> Maybe Switch
    switchControl c = (playback . switch =<< c)
              `mplus` (capture . switch =<< c)
              `mplus` (common . switch =<< c)

    liftMaybe :: Maybe (IO (a,b)) -> IO (Maybe a, Maybe b)
    liftMaybe = fmap (liftM2 (,) (fmap fst) (fmap snd)) . sequenceA

    liftMonitor :: Maybe (Monitor String) -> Monitor String
    liftMonitor Nothing = unavailable
    liftMonitor (Just m) = m

    channel' v r = AE.catch (getChannel FrontLeft v) (const $ return $ Just r)

    channel v r = channel' v r >>= \x -> return (x >>= Just . toInteger)

    getDB :: Maybe Volume -> IO (Maybe Integer)
    getDB Nothing = return Nothing
    getDB (Just v) = channel (dB v) 0

    getVal :: Maybe Volume -> IO (Maybe Integer)
    getVal Nothing = return Nothing
    getVal (Just v) = channel (value v) 0

    getSw :: Maybe Switch -> IO (Maybe Bool)
    getSw Nothing = return Nothing
    getSw (Just s) = channel' s False

    getFormatDB :: VolumeOpts -> Maybe Integer -> Monitor String
    getFormatDB _ Nothing = unavailable
    getFormatDB opts' (Just d) = formatDb opts' d

    getFormatSwitch :: VolumeOpts -> Maybe Bool -> Maybe VolumeStatus -> Monitor String
    getFormatSwitch _ Nothing _ = unavailable
    getFormatSwitch _ _ Nothing = unavailable
    getFormatSwitch opts' (Just sw) (Just vs) = formatSwitch opts' sw vs

    unavailable = getConfigValue naString
