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

module Xmobar.Plugins.Monitors.Bright (brightConfig, runBright) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, handle)
import qualified Data.ByteString.Lazy.Char8 as B
import System.FilePath ((</>))
import System.Posix.Files (fileExist)
import System.Console.GetOpt

import Xmobar.Plugins.Monitors.Common

data BrightOpts = BrightOpts { subDir :: String
                             , currBright :: String
                             , maxBright :: String
                             , curBrightIconPattern :: Maybe IconPattern
                             }

defaultOpts :: BrightOpts
defaultOpts = BrightOpts { subDir = "acpi_video0"
                         , currBright = "actual_brightness"
                         , maxBright = "max_brightness"
                         , curBrightIconPattern = Nothing
                         }

options :: [OptDescr (BrightOpts -> BrightOpts)]
options = [ Option "D" ["device"] (ReqArg (\x o -> o { subDir = x }) "") ""
          , Option "C" ["curr"] (ReqArg (\x o -> o { currBright = x }) "") ""
          , Option "M" ["max"] (ReqArg (\x o -> o { maxBright = x }) "") ""
          , Option "" ["brightness-icon-pattern"] (ReqArg (\x o ->
             o { curBrightIconPattern = Just $ parseIconPattern x }) "") ""
          ]

sysDir :: FilePath
sysDir = "/sys/class/backlight/"

brightConfig :: IO MConfig
brightConfig = mkMConfig "<percent>" -- template
                         ["vbar", "percent", "bar", "ipat"] -- replacements

data Files = Files { fCurr :: String
                   , fMax :: String
                   }
           | NoFiles

brightFiles :: BrightOpts -> IO Files
brightFiles opts = do
  is_curr <- fileExist $ fCurr files
  is_max  <- fileExist $ fCurr files
  return (if is_curr && is_max then files else NoFiles)
  where prefix = sysDir </> subDir opts
        files = Files { fCurr = prefix </> currBright opts
                      , fMax = prefix </> maxBright opts
                      }

runBright :: [String] ->  Monitor String
runBright args = do
  opts <- io $ parseOptsWith options defaultOpts args
  f <- io $ brightFiles opts
  c <- io $ readBright f
  case f of
    NoFiles -> return "hurz"
    _ -> fmtPercent opts c >>= parseTemplate
  where fmtPercent :: BrightOpts -> Float -> Monitor [String]
        fmtPercent opts c = do r <- showVerticalBar (100 * c) c
                               s <- showPercentWithColors c
                               t <- showPercentBar (100 * c) c
                               d <- showIconPattern (curBrightIconPattern opts) c
                               return [r,s,t,d]

readBright :: Files -> IO Float
readBright NoFiles = return 0
readBright files = do
  currVal<- grab $ fCurr files
  maxVal <- grab $ fMax files
  return (currVal / maxVal)
  where grab f = handle handler (read . B.unpack <$> B.readFile f)
        handler = const (return 0) :: SomeException -> IO Float

