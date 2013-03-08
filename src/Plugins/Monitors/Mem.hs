-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Mem
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A memory monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Mem (memConfig, runMem, totalMem, usedMem) where

import Plugins.Monitors.Common

memConfig :: IO MConfig
memConfig = mkMConfig
       "Mem: <usedratio>% (<cache>M)" -- template
       ["usedbar", "freebar", "usedratio", "freeratio", "total",
        "free", "buffer", "cache", "rest", "used"] -- available replacements

fileMEM :: IO String
fileMEM = readFile "/proc/meminfo"

parseMEM :: IO [Float]
parseMEM =
    do file <- fileMEM
       let content = map words $ take 4 $ lines file
           [total, free, buffer, cache] = map (\line -> (read $ line !! 1 :: Float) / 1024) content
           rest = free + buffer + cache
           used = total - rest
           usedratio = used / total
           freeratio = free / total
       return [usedratio, freeratio, total, free, buffer, cache, rest, used, freeratio]

totalMem :: IO Float
totalMem = fmap ((*1024) . (!!1)) parseMEM

usedMem :: IO Float
usedMem = fmap ((*1024) . (!!6)) parseMEM

formatMem :: [Float] -> Monitor [String]
formatMem (r:fr:xs) =
    do let f = showDigits 0
           rr = 100 * r
       ub <- showPercentBar rr r
       fb <- showPercentBar (100 - rr) (1 - r)
       rs <- showPercentWithColors r
       fs <- showPercentWithColors fr
       s <- mapM (showWithColors f) xs
       return (ub:fb:rs:fs:s)
formatMem _ = return $ replicate 10 "N/A"

runMem :: [String] -> Monitor String
runMem _ =
    do m <- io parseMEM
       l <- formatMem m
       parseTemplate l
