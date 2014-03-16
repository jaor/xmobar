-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Cpu
-- Copyright   :  (c) 2011 Jose Antonio Ortega Ruiz
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

module Plugins.Monitors.Cpu (startCpu) where

import Plugins.Monitors.Common
import qualified Data.ByteString.Lazy.Char8 as B
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

cpuConfig :: IO MConfig
cpuConfig = mkMConfig
       "Cpu: <total>%"
       ["bar","vbar","total","user","nice","system","idle","iowait"]

type CpuDataRef = IORef [Float]

cpuData :: IO [Float]
cpuData = cpuParser `fmap` B.readFile "/proc/stat"

cpuParser :: B.ByteString -> [Float]
cpuParser = map (read . B.unpack) . tail . B.words . head . B.lines

parseCpu :: CpuDataRef -> IO [Float]
parseCpu cref =
    do a <- readIORef cref
       b <- cpuData
       writeIORef cref b
       let dif = zipWith (-) b a
           tot = foldr (+) 0 dif
           percent = map (/ tot) dif
       return percent

formatCpu :: [Float] -> Monitor [String]
formatCpu [] = return $ replicate 8 ""
formatCpu xs = do
  let t = sum $ take 3 xs
  b <- showPercentBar (100 * t) t
  v <- showVerticalBar (100 * t) t
  ps <- showPercentsWithColors (t:xs)
  return (b:v:ps)

runCpu :: CpuDataRef -> [String] -> Monitor String
runCpu cref _ =
    do c <- io (parseCpu cref)
       l <- formatCpu c
       parseTemplate l

startCpu :: [String] -> Int -> (String -> IO ()) -> IO ()
startCpu a r cb = do
  cref <- newIORef []
  _ <- parseCpu cref
  runM a cpuConfig (runCpu cref) r cb
