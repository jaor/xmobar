------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Plugins.Monitors.Strings
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Dec 02, 2018 04:25
--
--
-- Utilities for formatting monitor outputs
--
------------------------------------------------------------------------------


module Xmobar.Plugins.Monitors.Common.Output ( IconPattern
                                             , parseIconPattern
                                             , padString
                                             , showWithPadding
                                             , showWithColors
                                             , showWithColors'
                                             , showPercentWithColors
                                             , showPercentsWithColors
                                             , showPercentBar
                                             , showVerticalBar
                                             , showIconPattern
                                             , showLogBar
                                             , showLogVBar
                                             , showLogIconPattern
                                             , showWithUnits
                                             , takeDigits
                                             , showDigits
                                             , floatToPercent
                                             , parseFloat
                                             , parseInt
                                             , stringParser
                                             ) where

import Data.Char
import Data.List (intercalate, sort)
import qualified Data.ByteString.Lazy.Char8 as B
import Numeric
import Control.Monad (zipWithM)

import Xmobar.Plugins.Monitors.Common.Types

type IconPattern = Int -> String

parseIconPattern :: String -> IconPattern
parseIconPattern path =
    let spl = splitOnPercent path
    in \i -> intercalate (show i) spl
  where splitOnPercent [] = [[]]
        splitOnPercent ('%':'%':xs) = [] : splitOnPercent xs
        splitOnPercent (x:xs) =
            let rest = splitOnPercent xs
            in (x : head rest) : tail rest

type Pos = (Int, Int)

takeDigits :: Int -> Float -> Float
takeDigits d n =
    fromIntegral (round (n * fact) :: Int) / fact
  where fact = 10 ^ d

showDigits :: (RealFloat a) => Int -> a -> String
showDigits d n = showFFloat (Just d) n ""

showWithUnits :: Int -> Int -> Float -> String
showWithUnits d n x
  | x < 0 = '-' : showWithUnits d n (-x)
  | n > 3 || x < 10^(d + 1) = show (round x :: Int) ++ units n
  | x <= 1024 = showDigits d (x/1024) ++ units (n+1)
  | otherwise = showWithUnits d (n+1) (x/1024)
  where units = (!!) ["B", "K", "M", "G", "T"]

padString :: Int -> Int -> String -> Bool -> String -> String -> String
padString mnw mxw pad pr ellipsis s =
  let len = length s
      rmin = if mnw <= 0 then 1 else mnw
      rmax = if mxw <= 0 then max len rmin else mxw
      (rmn, rmx) = if rmin <= rmax then (rmin, rmax) else (rmax, rmin)
      rlen = min (max rmn len) rmx
  in if rlen < len then
       take rlen s ++ ellipsis
     else let ps = take (rlen - len) (cycle pad)
          in if pr then s ++ ps else ps ++ s

parseFloat :: String -> Float
parseFloat s = case readFloat s of
  (v, _):_ -> v
  _ -> 0

parseInt :: String -> Int
parseInt s = case readDec s of
  (v, _):_ -> v
  _ -> 0

floatToPercent :: Float -> Monitor String
floatToPercent n =
  do pad <- getConfigValue ppad
     pc <- getConfigValue padChars
     pr <- getConfigValue padRight
     up <- getConfigValue useSuffix
     let p = showDigits 0 (n * 100)
         ps = if up then "%" else ""
     return $ padString pad pad pc pr "" p ++ ps

stringParser :: Pos -> B.ByteString -> String
stringParser (x,y) =
     B.unpack . li x . B.words . li y . B.lines
    where li i l | length l > i = l !! i
                 | otherwise    = B.empty

setColor :: String -> Selector (Maybe String) -> Monitor String
setColor str s =
    do a <- getConfigValue s
       case a of
            Nothing -> return str
            Just c -> return $
                "<fc=" ++ c ++ ">" ++ str ++ "</fc>"

showWithPadding :: String -> Monitor String
showWithPadding s =
    do mn <- getConfigValue minWidth
       mx <- getConfigValue maxWidth
       p <- getConfigValue padChars
       pr <- getConfigValue padRight
       ellipsis <- getConfigValue maxWidthEllipsis
       return $ padString mn mx p pr ellipsis s

colorizeString :: (Num a, Ord a) => a -> String -> Monitor String
colorizeString x s = do
    h <- getConfigValue high
    l <- getConfigValue low
    let col = setColor s
        [ll,hh] = map fromIntegral $ sort [l, h] -- consider high < low
    head $ [col highColor   | x > hh ] ++
           [col normalColor | x > ll ] ++
           [col lowColor    | True]

showWithColors :: (Num a, Ord a) => (a -> String) -> a -> Monitor String
showWithColors f x = showWithPadding (f x) >>= colorizeString x

showWithColors' :: (Num a, Ord a) => String -> a -> Monitor String
showWithColors' str = showWithColors (const str)

showPercentsWithColors :: [Float] -> Monitor [String]
showPercentsWithColors fs =
  do fstrs <- mapM floatToPercent fs
     zipWithM (showWithColors . const) fstrs (map (*100) fs)

showPercentWithColors :: Float -> Monitor String
showPercentWithColors f = fmap head $ showPercentsWithColors [f]

showPercentBar :: Float -> Float -> Monitor String
showPercentBar v x = do
  bb <- getConfigValue barBack
  bf <- getConfigValue barFore
  bw <- getConfigValue barWidth
  let len = min bw $ round (fromIntegral bw * x)
  s <- colorizeString v (take len $ cycle bf)
  return $ s ++ take (bw - len) (cycle bb)

showIconPattern :: Maybe IconPattern -> Float -> Monitor String
showIconPattern Nothing _ = return ""
showIconPattern (Just str) x = return $ str $ convert $ 100 * x
  where convert val
          | t <= 0 = 0
          | t > 8 = 8
          | otherwise = t
          where t = round val `div` 12

showVerticalBar :: Float -> Float -> Monitor String
showVerticalBar v x = colorizeString v [convert $ 100 * x]
  where convert :: Float -> Char
        convert val
          | t <= 9600 = ' '
          | t > 9608 = chr 9608
          | otherwise = chr t
          where t = 9600 + (round val `div` 12)

logScaling :: Float -> Float -> Monitor Float
logScaling f v = do
  h <- fromIntegral `fmap` getConfigValue high
  l <- fromIntegral `fmap` getConfigValue low
  bw <- fromIntegral `fmap` getConfigValue barWidth
  let [ll, hh] = sort [l, h]
      scaled x | x == 0.0 = 0
               | x <= ll = 1 / bw
               | otherwise = f + logBase 2 (x / hh) / bw
  return $ scaled v

showLogBar :: Float -> Float -> Monitor String
showLogBar f v = logScaling f v >>= showPercentBar v

showLogVBar :: Float -> Float -> Monitor String
showLogVBar f v = logScaling f v >>= showVerticalBar v

showLogIconPattern :: Maybe IconPattern -> Float -> Float -> Monitor String
showLogIconPattern str f v = logScaling f v >>= showIconPattern str
