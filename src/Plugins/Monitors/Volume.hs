-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Volume
-- Copyright   :  (c) 2011 Thomas Tuegel
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A monitor for ALSA soundcards
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Volume (runVolume, volumeConfig) where

import Control.Applicative ((<$>))
import Control.Monad ( join, liftM2, liftM3, mplus )
import Data.Traversable (sequenceA)
import Plugins.Monitors.Common
import Sound.ALSA.Mixer
import qualified Sound.ALSA.Exception as AE
import System.Console.GetOpt

volumeConfig :: IO MConfig
volumeConfig = mkMConfig "Vol: <volume>% <status>"
                         ["volume", "volumebar", "dB","status"]


data VolumeOpts = VolumeOpts
    { onString :: String
    , offString :: String
    , onColor :: Maybe String
    , offColor :: Maybe String
    , highDbThresh :: Float
    , lowDbThresh :: Float
    }

defaultOpts :: VolumeOpts
defaultOpts = VolumeOpts
    { onString = "[on] "
    , offString = "[off]"
    , onColor = Just "green"
    , offColor = Just "red"
    , highDbThresh = -5.0
    , lowDbThresh = -30.0
    }

options :: [OptDescr (VolumeOpts -> VolumeOpts)]
options =
    [ Option "O" ["on"] (ReqArg (\x o -> o { onString = x }) "") ""
    , Option "o" ["off"] (ReqArg (\x o -> o { offString = x }) "") ""
    , Option "" ["lowd"] (ReqArg (\x o -> o { lowDbThresh = read x }) "") ""
    , Option "" ["highd"] (ReqArg (\x o -> o { highDbThresh = read x }) "") ""
    , Option "C" ["onc"] (ReqArg (\x o -> o { onColor = Just x }) "") ""
    , Option "c" ["offc"] (ReqArg (\x o -> o { offColor = Just x }) "") ""
    ]

parseOpts :: [String] -> IO VolumeOpts
parseOpts argv =
    case getOpt Permute options argv of
        (o, _, []) -> return $ foldr id defaultOpts o
        (_, _, errs) -> ioError . userError $ concat errs

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

switchHelper :: VolumeOpts
             -> (VolumeOpts -> Maybe String)
             -> (VolumeOpts -> String)
             -> Monitor String
switchHelper opts cHelp strHelp = return $
    colorHelper (cHelp opts)
    ++ strHelp opts
    ++ maybe "" (const "</fc>") (cHelp opts)

formatSwitch :: VolumeOpts -> Bool -> Monitor String
formatSwitch opts True = switchHelper opts onColor onString
formatSwitch opts False = switchHelper opts offColor offString

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
    opts <- io $ parseOpts argv
    control <- io $ getControlByName mixerName controlName
    (lo, hi) <- io . liftMaybe $ getRange <$> volumeControl control
    val <- getVal $ volumeControl control
    db <- getDB $ volumeControl control
    sw <- getSw $ switchControl control
    p <- liftMonitor $ liftM3 formatVol lo hi val
    b <- liftMonitor $ liftM3 formatVolBar lo hi val
    d <- getFormatDB opts db
    s <- getFormatSwitch opts sw
    parseTemplate [p, b, d, s]

  where

    volumeControl :: Maybe Control -> Maybe Volume
    volumeControl c = join $
           (playback . volume <$> c) `mplus` (common . volume <$> c)

    switchControl :: Maybe Control -> Maybe Switch
    switchControl c = join $
           (playback . switch <$> c) `mplus` (common . switch <$> c)

    liftMaybe :: Maybe (IO (a,b)) -> IO (Maybe a, Maybe b)
    liftMaybe = fmap (liftM2 (,) (fmap fst) (fmap snd)) . sequenceA

    liftMonitor :: Maybe (Monitor String) -> Monitor String
    liftMonitor Nothing = return unavailable
    liftMonitor (Just m) = m

    getDB :: Maybe Volume -> Monitor (Maybe Integer)
    getDB Nothing = return Nothing
    getDB (Just v) = io $ AE.catch (getChannel FrontLeft $ dB v)
                                   (const $ return $ Just 0)

    getVal :: Maybe Volume -> Monitor (Maybe Integer)
    getVal Nothing = return Nothing
    getVal (Just v) = io $ getChannel FrontLeft $ value v

    getSw :: Maybe Switch -> Monitor (Maybe Bool)
    getSw Nothing = return Nothing
    getSw (Just s) = io $ getChannel FrontLeft s

    getFormatDB :: VolumeOpts -> Maybe Integer -> Monitor String
    getFormatDB _ Nothing = return unavailable
    getFormatDB opts (Just d) = formatDb opts d

    getFormatSwitch :: VolumeOpts -> Maybe Bool -> Monitor String
    getFormatSwitch _ Nothing = return unavailable
    getFormatSwitch opts (Just sw) = formatSwitch opts sw

    unavailable = "N/A"
