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

import Prelude hiding ( catch )
import Control.Monad ( liftM, mplus )
import Data.Maybe
import Plugins.Monitors.Common
import Sound.ALSA.Mixer
import Sound.ALSA.Exception ( catch )
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
    let db = fromIntegral dbi / 100.0
        digits = showDigits 0 db
        startColor | db >= highDbThresh opts = colorHelper h
                   | db < lowDbThresh opts = colorHelper l
                   | otherwise = colorHelper m
        stopColor | null startColor = ""
                  | otherwise = "</fc>"
    return $ startColor ++ digits ++ stopColor

runVolume :: String -> String -> [String] -> Monitor String
runVolume mixerName controlName argv = do
    opts <- io $ parseOpts argv
    control <- liftM fromJust $ io $ getControlByName mixerName controlName
    let volumeControl = fromJust $ mplus (playback $ volume control)
                                         (common $ volume control)
        switchControl = fromJust $ mplus (playback $ switch control)
                                         (common $ switch control)
        maybeNA = maybe (return "N/A")
    (lo, hi) <- io $ getRange volumeControl
    val <- io $ getChannel FrontLeft $ value volumeControl
    db <- io $ catch (getChannel FrontLeft $ dB volumeControl)
                     (\_ -> return $ Just 0)
    sw <- io $ getChannel FrontLeft switchControl
    p <- maybeNA (formatVol lo hi) val
    b <- maybeNA (formatVolBar lo hi) val
    d <- maybeNA (formatDb opts) db
    s <- maybeNA (formatSwitch opts) sw
    parseTemplate [p, b, d, s]
