-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Swap
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A  swap usage monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Swap where

import Plugins.Monitors.Common

import qualified Data.ByteString.Lazy.Char8 as B

swapConfig :: IO MConfig
swapConfig = mkMConfig
        "Swap: <usedratio>%"                    -- template
        ["usedratio", "total", "used", "free"] -- available replacements

fileMEM :: IO B.ByteString
fileMEM = B.readFile "/proc/meminfo"

parseMEM :: IO [Float]
parseMEM =
    do file <- fileMEM
       let li i l
               | l /= [] = head l !! i
               | otherwise = B.empty
           fs s l
               | null l    = False
               | otherwise = head l == B.pack s
           get_data s = flip (/) 1024 . read . B.unpack . li 1 . filter (fs s)
           st   = map B.words . B.lines $ file
           tot  = get_data "SwapTotal:" st
           free = get_data "SwapFree:" st
       return [(tot - free) / tot, tot, tot - free, free]

formatSwap :: [Float] -> Monitor [String]
formatSwap (r:xs) = do
  d <- getConfigValue decDigits
  other <- mapM (showWithColors (showDigits d)) xs
  ratio <- showPercentWithColors r
  return $ ratio:other
formatSwap _ = return $ replicate 4 "N/A"

runSwap :: [String] -> Monitor String
runSwap _ =
    do m <- io parseMEM
       l <- formatSwap m
       parseTemplate l
