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

module Plugins.Monitors.MultiCpu (startMultiCpu) where

import Plugins.Monitors.Common
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (isPrefixOf, transpose, unfoldr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

variables :: [String]
variables = ["bar", "vbar","total","user","nice","system","idle"]
vNum :: Int
vNum = length variables

multiCpuConfig :: IO MConfig
multiCpuConfig =
  mkMConfig "Cpu: <total>%" $
            ["auto" ++ k | k <- variables] ++
            [ k ++ n     | n <- "" : map show [0 :: Int ..]
                         , k <- variables]

type CpuDataRef = IORef [[Float]]

cpuData :: IO [[Float]]
cpuData = parse `fmap` B.readFile "/proc/stat"
  where parse = map parseList . cpuLists
        cpuLists = takeWhile isCpu . map B.words . B.lines
        isCpu (w:_) = "cpu" `isPrefixOf` B.unpack w
        isCpu _ = False
        parseList = map (parseFloat . B.unpack) . tail

parseCpuData :: CpuDataRef -> IO [[Float]]
parseCpuData cref =
  do as <- readIORef cref
     bs <- cpuData
     writeIORef cref bs
     let p0 = zipWith percent bs as
     return p0

percent :: [Float] -> [Float] -> [Float]
percent b a = if tot > 0 then map (/ tot) $ take 4 dif else [0, 0, 0, 0]
  where dif = zipWith (-) b a
        tot = foldr (+) 0 dif

formatMultiCpus :: [[Float]] -> Monitor [String]
formatMultiCpus [] = return []
formatMultiCpus xs = fmap concat $ mapM formatCpu xs

formatCpu :: [Float] -> Monitor [String]
formatCpu xs
  | length xs < 4 = showPercentsWithColors $ replicate vNum 0.0
  | otherwise = let t = foldr (+) 0 $ take 3 xs
                in do b <- showPercentBar (100 * t) t
                      h <- showVerticalBar (100 * t) t
                      ps <- showPercentsWithColors (t:xs)
                      return (b:h:ps)

splitEvery :: (Eq a) => Int -> [a] -> [[a]]
splitEvery n = unfoldr (\x -> if null x then Nothing else Just $ splitAt n x)

groupData :: [String] -> [[String]]
groupData = transpose . tail . splitEvery vNum

formatAutoCpus :: [String] -> Monitor [String]
formatAutoCpus [] = return $ replicate vNum ""
formatAutoCpus xs = return $ map unwords (groupData xs)

runMultiCpu :: CpuDataRef -> [String] -> Monitor String
runMultiCpu cref _ =
  do c <- io $ parseCpuData cref
     l <- formatMultiCpus c
     a <- formatAutoCpus l
     parseTemplate $ a ++ l

startMultiCpu :: [String] -> Int -> (String -> IO ()) -> IO ()
startMultiCpu a r cb = do
  cref <- newIORef [[]]
  _ <- parseCpuData cref
  runM a multiCpuConfig (runMultiCpu cref) r cb
