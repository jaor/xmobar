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

import Plugins.Monitors.Common
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import System.FilePath ((</>))
import System.Posix.Files (fileExist)
import System.Console.GetOpt

data BrightOpts = BrightOpts
	{ subDir :: String
	, currBright :: String
	, maxBright :: String
	}

defaultOpts :: BrightOpts
defaultOpts = BrightOpts
	{ subDir = "acpi_video0"
	, currBright = "actual_brightness"
	, maxBright = "max_brightness"
	}

options :: [OptDescr (BrightOpts -> BrightOpts)]
options =
	[ Option "D" ["device"] (ReqArg (\x o -> o { subDir = x }) "") ""
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
brightConfig = mkMConfig
		"<percent>" -- template
		["hbar", "percent", "bar"] -- replacements

data Files = Files
	{ fCurr :: String
	, fMax :: String
	} | NoFiles

brightFiles :: BrightOpts -> IO Files
brightFiles opts =
	do
		is_curr <- fileExist $ (fCurr files)
		is_max  <- fileExist $ (fCurr files)
		if is_curr && is_max
			then return files
			else return NoFiles
	where
		prefix = sysDir </> (subDir opts)
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
		_ -> do x <- fmtPercent c
			parseTemplate (x)
	where
		fmtPercent :: Float -> Monitor [String]
		fmtPercent c = do
			r <- showHorizontalBar (100 * c)
			s <- showPercentWithColors c
			t <- showPercentBar (100 * c) c
			return [r,s,t]

readBright :: Files -> IO Float
readBright NoFiles = return 0
readBright files =
	do
		currVal<- grab $ (fCurr files)
		maxVal <- grab $ (fMax files)
		return $ (currVal / maxVal) 
	where
		grab f = catch (fmap (read . B.unpack) $ B.readFile f)(\_ -> return 0)


showHorizontalBar :: Float -> Monitor String
showHorizontalBar x = do
	return $ [convert x]
	where
		convert :: Float -> Char
		convert val 
			| t <= 9600 = ' '
			| t > 9608 = chr 9608
			| otherwise = chr t
			where
				-- we scale from 0 to 100, we have 8 slots (9 elements), 100/8 = 12
				t = 9600 + ((round val) `div` 12)
