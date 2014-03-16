-----------------------------------------------------------------------------
---- |
---- Module      :  Plugins.Monitors.Birght
---- Copyright   :  (c) Martin Perner
---- License     :  BSD-style (see LICENSE)
----
---- Maintainer  :  Martin Perner <martin@perner.cc>
---- Stability   :  unstable
---- Portability :  unportable
----
----  A screen brightness monitor for Xmobar
----
-------------------------------------------------------------------------------

module Plugins.Monitors.Bright (brightConfig, runBright) where

import Control.Exception (SomeException, handle)
import qualified Data.ByteString.Lazy.Char8 as B
import System.FilePath ((</>))
import System.Posix.Files (fileExist)
import System.Console.GetOpt

import Plugins.Monitors.Common

data BrightOpts = BrightOpts { subDir :: String
                             , currBright :: String
                             , maxBright :: String
                             }

defaultOpts :: BrightOpts
defaultOpts = BrightOpts { subDir = "acpi_video0"
                         , currBright = "actual_brightness"
                         , maxBright = "max_brightness"
                         }

options :: [OptDescr (BrightOpts -> BrightOpts)]
options = [ Option "D" ["device"] (ReqArg (\x o -> o { subDir = x }) "") ""
          , Option "C" ["curr"] (ReqArg (\x o -> o { currBright = x }) "") ""
          , Option "M" ["max"] (ReqArg (\x o -> o { maxBright = x }) "") ""
          ]

-- from Batt.hs
parseOpts :: [String] -> IO BrightOpts
parseOpts argv =
  case getOpt Permute options argv of
    (o, _, []) -> return $ foldr id defaultOpts o
    (_, _, errs) -> ioError . userError $ concat errs

sysDir :: FilePath
sysDir = "/sys/class/backlight/"

brightConfig :: IO MConfig
brightConfig = mkMConfig "<percent>" -- template
                         ["vbar", "percent", "bar"] -- replacements

data Files = Files { fCurr :: String
                   , fMax :: String
                   }
           | NoFiles

brightFiles :: BrightOpts -> IO Files
brightFiles opts = do
  is_curr <- fileExist $ (fCurr files)
  is_max  <- fileExist $ (fCurr files)
  if is_curr && is_max then return files else return NoFiles
  where prefix = sysDir </> (subDir opts)
        files = Files { fCurr = prefix </> (currBright opts)
                      , fMax = prefix </> (maxBright opts)
                      }

runBright :: [String] ->  Monitor String
runBright args = do
  opts <- io $ parseOpts args
  f <- io $ brightFiles opts
  c <- io $ readBright f
  case f of
    NoFiles -> return "hurz"
    _ -> fmtPercent c >>= parseTemplate
  where fmtPercent :: Float -> Monitor [String]
        fmtPercent c = do r <- showVerticalBar (100 * c) c
                          s <- showPercentWithColors c
                          t <- showPercentBar (100 * c) c
                          return [r,s,t]

readBright :: Files -> IO Float
readBright NoFiles = return 0
readBright files = do
  currVal<- grab $ (fCurr files)
  maxVal <- grab $ (fMax files)
  return $ (currVal / maxVal)
  where grab f = handle handler (fmap (read . B.unpack) $ B.readFile f)
        handler = const (return 0) :: SomeException -> IO Float

