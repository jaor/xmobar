-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Batt
-- Copyright   :  (c) 2010, 2011 Jose A Ortega
--                (c) 2010 Andrea Rossato, Petr Rockai
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A battery monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Batt ( battConfig, runBatt, runBatt' ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Plugins.Monitors.Common
import System.FilePath ((</>))
import System.Posix.Files (fileExist)
import System.Console.GetOpt

data BattOpts = BattOpts
  { onString :: String
  , offString :: String
  , posColor :: Maybe String
  , lowWColor :: Maybe String
  , mediumWColor :: Maybe String
  , highWColor :: Maybe String
  , lowThreshold :: Float
  , highThreshold :: Float
  , onlineFile :: FilePath
  }

defaultOpts :: BattOpts
defaultOpts = BattOpts
  { onString = "On"
  , offString = "Off"
  , posColor = Nothing
  , lowWColor = Nothing
  , mediumWColor = Nothing
  , highWColor = Nothing
  , lowThreshold = -12
  , highThreshold = -10
  , onlineFile = "AC/online"
  }

options :: [OptDescr (BattOpts -> BattOpts)]
options =
  [ Option "O" ["on"] (ReqArg (\x o -> o { onString = x }) "") ""
  , Option "o" ["off"] (ReqArg (\x o -> o { offString = x }) "") ""
  , Option "p" ["positive"] (ReqArg (\x o -> o { posColor = Just x }) "") ""
  , Option "l" ["low"] (ReqArg (\x o -> o { lowWColor = Just x }) "") ""
  , Option "m" ["medium"] (ReqArg (\x o -> o { mediumWColor = Just x }) "") ""
  , Option "h" ["high"] (ReqArg (\x o -> o { highWColor = Just x }) "") ""
  , Option "L" ["lowt"] (ReqArg (\x o -> o { lowThreshold = read x }) "") ""
  , Option "H" ["hight"] (ReqArg (\x o -> o { highThreshold = read x }) "") ""
  , Option "f" ["online"] (ReqArg (\x o -> o { onlineFile = x }) "") ""
  ]

parseOpts :: [String] -> IO BattOpts
parseOpts argv =
  case getOpt Permute options argv of
    (o, _, []) -> return $ foldr id defaultOpts o
    (_, _, errs) -> ioError . userError $ concat errs

data Result = Result Float Float Float String | NA

sysDir :: FilePath
sysDir = "/sys/class/power_supply"

battConfig :: IO MConfig
battConfig = mkMConfig
       "Batt: <watts>, <left>% / <timeleft>" -- template
       ["leftbar", "left", "acstatus", "timeleft", "watts"] -- replacements

data Files = Files
  { fFull :: String
  , fNow :: String
  , fVoltage :: String
  , fCurrent :: String
  } | NoFiles

data Battery = Battery
  { full :: Float
  , now :: Float
  , voltage :: Float
  , current :: Float
  }

batteryFiles :: String -> IO Files
batteryFiles bat =
  do is_charge <- fileExist $ prefix </> "charge_now"
     is_energy <- fileExist $ prefix </> "energy_now"
     is_current <- fileExist $ prefix </> "current_now"
     let cf = if is_current then "current_now" else "power_now"
     return $ case (is_charge, is_energy) of
       (True, _) -> files "charge" cf
       (_, True) -> files "energy" cf
       _ -> NoFiles
  where prefix = sysDir </> bat
        files ch cf = Files { fFull = prefix </> ch ++ "_full"
                            , fNow = prefix </> ch ++ "_now"
                            , fCurrent = prefix </> cf
                            , fVoltage = prefix </> "voltage_now" }

haveAc :: FilePath -> IO Bool
haveAc f = do
  exists <- fileExist ofile
  if exists
    then fmap ((== "1\n") . B.unpack) (B.readFile ofile)
    else return False
  where ofile = sysDir </> f

readBattery :: Files -> IO Battery
readBattery NoFiles = return $ Battery 0 0 0 0
readBattery files =
    do a <- grab $ fFull files -- microwatthours
       b <- grab $ fNow files
       c <- grab $ fVoltage files -- microvolts
       d <- grab $ fCurrent files -- microwatts (huh!)
       return $ Battery (3600 * a / 1000000) -- wattseconds
                        (3600 * b / 1000000) -- wattseconds
                        (c / 1000000) -- volts
                        (if c > 0 then (d / c) else -1) -- amperes
    where grab f = catch (fmap (read . B.unpack) $ B.readFile f) (\_ -> return 0)

readBatteries :: BattOpts -> [Files] -> IO Result
readBatteries opts bfs =
    do bats <- mapM readBattery (take 3 bfs)
       ac <- haveAc (onlineFile opts)
       let sign = if ac then 1 else -1
           ft = sum (map full bats)
           left = if ft > 0 then sum (map now bats) / ft else 0
           watts = sign * sum (map voltage bats) * sum (map current bats)
           time = if watts == 0 then 0 else sum $ map time' bats
           mwatts = if watts == 0 then 1 else sign * watts
           time' b = (if ac then full b - now b else now b) / mwatts
           acstr = if ac then onString opts else offString opts
       return $ if isNaN left then NA else Result left watts time acstr

runBatt :: [String] -> Monitor String
runBatt = runBatt' ["BAT0","BAT1","BAT2"]

runBatt' :: [String] -> [String] -> Monitor String
runBatt' bfs args = do
  opts <- io $ parseOpts args
  c <- io $ readBatteries opts =<< mapM batteryFiles bfs
  case c of
    Result x w t s ->
      do l <- fmtPercent x
         parseTemplate (l ++ s:[fmtTime $ floor t, fmtWatts w opts])
    NA -> return "N/A"
 where fmtPercent :: Float -> Monitor [String]
       fmtPercent x = do
         p <- showPercentWithColors x
         b <- showPercentBar (100 * x) x
         return [b, p]
       fmtWatts x o = color x o $ showDigits 1 x ++ "W"
       fmtTime :: Integer -> String
       fmtTime x = hours ++ ":" ++ if length minutes == 2
                                   then minutes else '0' : minutes
         where hours = show (x `div` 3600)
               minutes = show ((x `mod` 3600) `div` 60)
       maybeColor Nothing _ = ""
       maybeColor (Just c) str = "<fc=" ++ c ++ ">" ++ str ++ "</fc>"
       color x o | x >= 0 = maybeColor (posColor o)
                 | x >= highThreshold o = maybeColor (highWColor o)
                 | x >= lowThreshold o = maybeColor (mediumWColor o)
                 | otherwise = maybeColor (lowWColor o)
