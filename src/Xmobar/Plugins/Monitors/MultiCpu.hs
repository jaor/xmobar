-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.MultiCpu
-- Copyright   :  (c) Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A multi-cpu monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.MultiCpu (startMultiCpu) where

import Xmobar.Plugins.Monitors.Common
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (isPrefixOf, transpose, unfoldr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Console.GetOpt

data MultiCpuOpts = MultiCpuOpts
  { loadIconPatterns :: [IconPattern]
  , loadIconPattern :: Maybe IconPattern
  , fallbackIconPattern :: Maybe IconPattern
  , contiguous :: Bool
  }

defaultOpts :: MultiCpuOpts
defaultOpts = MultiCpuOpts
  { loadIconPatterns = []
  , loadIconPattern = Nothing
  , fallbackIconPattern = Nothing
  , contiguous = False
  }

options :: [OptDescr (MultiCpuOpts -> MultiCpuOpts)]
options =
  [ Option "" ["load-icon-pattern"] (ReqArg (\x o ->
     o { loadIconPattern = Just $ parseIconPattern x }) "") ""
  , Option "" ["load-icon-patterns"] (ReqArg (\x o ->
     o { loadIconPatterns = parseIconPattern x : loadIconPatterns o }) "") ""
  , Option "" ["fallback-icon-pattern"] (ReqArg (\x o ->
     o { fallbackIconPattern = Just $ parseIconPattern x }) "") ""
  , Option "" ["contiguous-icons"] (NoArg (\o -> o {contiguous = True})) ""
  ]

variables :: [String]
variables = ["bar", "vbar","ipat","total","user","nice","system","idle"]
vNum :: Int
vNum = length variables

multiCpuConfig :: IO MConfig
multiCpuConfig =
  mkMConfig "Cpu: <total>%" $
            ["auto" ++ k | k <- variables] ++
            [ k ++ n     | n <- "" : map show [0 :: Int ..]
                         , k <- variables]

type CpuDataRef = IORef [[Int]]

cpuData :: IO [[Int]]
cpuData = parse `fmap` B.readFile "/proc/stat"
  where parse = map parseList . cpuLists
        cpuLists = takeWhile isCpu . map B.words . B.lines
        isCpu (w:_) = "cpu" `isPrefixOf` B.unpack w
        isCpu _ = False
        parseList = map (parseInt . B.unpack) . tail

parseCpuData :: CpuDataRef -> IO [[Float]]
parseCpuData cref =
  do as <- readIORef cref
     bs <- cpuData
     writeIORef cref bs
     let p0 = zipWith percent bs as
     return p0

percent :: [Int] -> [Int] -> [Float]
percent b a = if tot > 0 then map (/ tot) $ take 4 dif else [0, 0, 0, 0]
  where dif = map fromIntegral $ zipWith (-) b a
        tot = sum dif

formatMultiCpus :: MultiCpuOpts -> [[Float]] -> Monitor [String]
formatMultiCpus _ [] = return []
formatMultiCpus opts xs =
  concat <$> mapM (\(i, x) -> formatCpu opts i x) (zip [0..] xs)

formatCpu :: MultiCpuOpts -> Int -> [Float] -> Monitor [String]
formatCpu opts i xs
  | length xs < 4 = showPercentsWithColors $ replicate vNum 0.0
  | otherwise = let t = sum $ take 3 xs
                in do b <- showPercentBar (100 * t) t
                      h <- showVerticalBar (100 * t) t
                      d <- showIconPattern tryString t
                      ps <- showPercentsWithColors (t:xs)
                      return (b:h:d:ps)
  where tryString
          | i == 0 = loadIconPattern opts
          | i <= length (loadIconPatterns opts) =
              Just $ loadIconPatterns opts !! (i - 1)
          | otherwise = fallbackIconPattern opts

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = unfoldr (\x -> if null x then Nothing else Just $ splitAt n x)

groupData :: [String] -> [[String]]
groupData = transpose . tail . splitEvery vNum

formatAutoCpus :: MultiCpuOpts -> [String] -> Monitor [String]
formatAutoCpus _ [] = return $ replicate vNum ""
formatAutoCpus opts xs =
  return $ map (if contiguous opts then concat else unwords) (groupData xs)

runMultiCpu :: CpuDataRef -> [String] -> Monitor String
runMultiCpu cref argv =
  do c <- io $ parseCpuData cref
     opts <- io $ parseOptsWith options defaultOpts argv
     l <- formatMultiCpus opts c
     a <- formatAutoCpus opts l
     parseTemplate $ a ++ l

startMultiCpu :: [String] -> Int -> (String -> IO ()) -> IO ()
startMultiCpu a r cb = do
  cref <- newIORef [[]]
  _ <- parseCpuData cref
  runM a multiCpuConfig (runMultiCpu cref) r cb
