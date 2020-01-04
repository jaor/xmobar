-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Cpu
-- Copyright   :  (c) 2011, 2017 Jose Antonio Ortega Ruiz
--                (c) 2007-2010 Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A cpu monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Cpu (startCpu) where

import Xmobar.Plugins.Monitors.Common
import qualified Data.ByteString.Lazy.Char8 as B
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Console.GetOpt

newtype CpuOpts = CpuOpts
  { loadIconPattern :: Maybe IconPattern
  }

defaultOpts :: CpuOpts
defaultOpts = CpuOpts
  { loadIconPattern = Nothing
  }

options :: [OptDescr (CpuOpts -> CpuOpts)]
options =
  [ Option "" ["load-icon-pattern"] (ReqArg (\x o ->
     o { loadIconPattern = Just $ parseIconPattern x }) "") ""
  ]

cpuConfig :: IO MConfig
cpuConfig = mkMConfig
       "Cpu: <total>%"
       ["bar","vbar","ipat","total","user","nice","system","idle","iowait"]

type CpuDataRef = IORef [Int]

cpuData :: IO [Int]
cpuData = cpuParser `fmap` B.readFile "/proc/stat"

cpuParser :: B.ByteString -> [Int]
cpuParser = map (read . B.unpack) . tail . B.words . head . B.lines

parseCpu :: CpuDataRef -> IO [Float]
parseCpu cref =
    do a <- readIORef cref
       b <- cpuData
       writeIORef cref b
       let dif = zipWith (-) b a
           tot = fromIntegral $ sum dif
           percent = map ((/ tot) . fromIntegral) dif
       return percent

formatCpu :: CpuOpts -> [Float] -> Monitor [String]
formatCpu _ [] = return $ replicate 8 ""
formatCpu opts xs = do
  let t = sum $ take 3 xs
  b <- showPercentBar (100 * t) t
  v <- showVerticalBar (100 * t) t
  d <- showIconPattern (loadIconPattern opts) t
  ps <- showPercentsWithColors (t:xs)
  return (b:v:d:ps)

runCpu :: CpuDataRef -> [String] -> Monitor String
runCpu cref argv =
    do c <- io (parseCpu cref)
       opts <- io $ parseOptsWith options defaultOpts argv
       l <- formatCpu opts c
       parseTemplate l

startCpu :: [String] -> Int -> (String -> IO ()) -> IO ()
startCpu a r cb = do
  cref <- newIORef []
  _ <- parseCpu cref
  runM a cpuConfig (runCpu cref) r cb
