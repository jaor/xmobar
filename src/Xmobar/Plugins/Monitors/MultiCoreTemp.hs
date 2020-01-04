-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.MultiCoreTemp
-- Copyright   :  (c) 2019 Felix Springer
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Felix Springer <felixspringer149@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A core temperature monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.MultiCoreTemp (startMultiCoreTemp) where

import Xmobar.Plugins.Monitors.Common
import Control.Monad (filterM)
import System.Console.GetOpt
import System.Directory ( doesDirectoryExist
                        , doesFileExist
                        )

-- | Declare Options.
data CTOpts = CTOpts { maxIconPattern :: Maybe IconPattern
                     , avgIconPattern :: Maybe IconPattern
                     , mintemp :: Float
                     , maxtemp :: Float
                     }

-- | Set default Options.
defaultOpts :: CTOpts
defaultOpts = CTOpts { maxIconPattern = Nothing
                     , avgIconPattern = Nothing
                     , mintemp = 0
                     , maxtemp = 100
                     }

-- | Apply configured Options.
options :: [OptDescr (CTOpts -> CTOpts)]
options = [ Option [] ["max-icon-pattern"]
              (ReqArg
                (\ arg opts -> opts { maxIconPattern = Just $ parseIconPattern arg })
                "")
              ""
          , Option [] ["avg-icon-pattern"]
              (ReqArg
                (\ arg opts -> opts { avgIconPattern = Just $ parseIconPattern arg })
                "")
              ""
          , Option [] ["mintemp"]
              (ReqArg
                (\ arg opts -> opts { mintemp = read arg })
                "")
              ""
          , Option [] ["maxtemp"]
              (ReqArg
                (\ arg opts -> opts { maxtemp = read arg })
                "")
              ""
          ]

-- | Generate Config with a default template and options.
cTConfig :: IO MConfig
cTConfig = mkMConfig cTTemplate cTOptions
  where cTTemplate = "Temp: <max>°C - <maxpc>%"
        cTOptions = [ "max" , "maxpc" , "maxbar" , "maxvbar" , "maxipat"
                    , "avg" , "avgpc" , "avgbar" , "avgvbar" , "avgipat"
                    ] ++ map (("core" ++) . show) [0 :: Int ..]

-- | Returns the first coretemp.N path found.
coretempPath :: IO String
coretempPath = do xs <- filterM doesDirectoryExist ps
                  let x = head xs
                  return x
  where ps = [ "/sys/bus/platform/devices/coretemp." ++ show (x :: Int) ++ "/" | x <- [0..9] ]

-- | Returns the first hwmonN path found.
hwmonPath :: IO String
hwmonPath = do p <- coretempPath
               xs <- filterM doesDirectoryExist [ p ++ "hwmon/hwmon" ++ show (x :: Int) ++ "/" | x <- [0..9] ]
               let x = head xs
               return x

-- | Checks Labels, if they refer to a core and returns Strings of core-
-- temperatures.
corePaths :: IO [String]
corePaths = do p <- hwmonPath
               ls <- filterM doesFileExist [ p ++ "temp" ++ show (x :: Int) ++ "_label" | x <- [0..9] ]
               cls <- filterM isLabelFromCore ls
               return $ map labelToCore cls

-- | Checks if Label refers to a core.
isLabelFromCore :: FilePath -> IO Bool
isLabelFromCore p = do a <- readFile p
                       return $ take 4 a == "Core"

-- | Transform a path to Label to a path to core-temperature.
labelToCore :: FilePath -> FilePath
labelToCore = (++ "input") . reverse . drop 5 . reverse

-- | Reads core-temperatures as data from the system.
cTData :: IO [Float]
cTData = do fps <- corePaths
            traverse readSingleFile fps
  where readSingleFile :: FilePath -> IO Float
        readSingleFile s = do a <- readFile s
                              return $ parseContent a
          where parseContent :: String -> Float
                parseContent = read . head . lines

-- | Transforms data of temperatures into temperatures of degree Celsius.
parseCT :: IO [Float]
parseCT = do rawCTs <- cTData
             let normalizedCTs = map (/ 1000) rawCTs :: [Float]
             return normalizedCTs

-- | Performs calculation for maximum and average.
-- Sets up Bars and Values to be printed.
formatCT :: CTOpts -> [Float] -> Monitor [String]
formatCT opts cTs = do let CTOpts { mintemp = minT
                                  , maxtemp = maxT } = opts
                           domainT = maxT - minT
                           maxCT = maximum cTs
                           avgCT = sum cTs / fromIntegral (length cTs)
                           calcPc t = (t - minT) / domainT
                           maxCTPc = calcPc maxCT
                           avgCTPc = calcPc avgCT

                       cs <- traverse showTempWithColors cTs

                       m <- showTempWithColors maxCT
                       mp <- showWithColors' (show (round (100*maxCTPc) :: Int)) maxCT
                       mb <- showPercentBar maxCT maxCTPc
                       mv <- showVerticalBar maxCT maxCTPc
                       mi <- showIconPattern (maxIconPattern opts) maxCTPc

                       a <- showTempWithColors avgCT
                       ap <- showWithColors' (show (round (100*avgCTPc) :: Int)) avgCT
                       ab <- showPercentBar avgCT avgCTPc
                       av <- showVerticalBar avgCT avgCTPc
                       ai <- showIconPattern (avgIconPattern opts) avgCTPc

                       let ms = [ m , mp , mb , mv , mi ]
                           as = [ a , ap , ab , av , ai ]

                       return (ms ++ as ++ cs)
  where showTempWithColors :: Float -> Monitor String
        showTempWithColors = showWithColors (show . (round :: Float -> Int))


runCT :: [String] -> Monitor String
runCT argv = do cTs <- io parseCT
                opts <- io $ parseOptsWith options defaultOpts argv
                l <- formatCT opts cTs
                parseTemplate l

startMultiCoreTemp :: [String] -> Int -> (String -> IO ()) -> IO ()
startMultiCoreTemp a = runM a cTConfig runCT
