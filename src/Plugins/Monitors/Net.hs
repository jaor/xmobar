-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Net
-- Copyright   :  (c) 2011, 2012 Jose Antonio Ortega Ruiz
--                (c) 2007-2010 Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A net device monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Net (
                        startNet
                      , startDynNet
                      ) where

import Plugins.Monitors.Common

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Control.Monad (forM, filterM)
import System.Directory (getDirectoryContents, doesFileExist)
import System.FilePath ((</>))

import qualified Data.ByteString.Lazy.Char8 as B

data NetDev = NA
            | NI String
            | ND String Float Float deriving (Eq,Show,Read)

type NetDevRef = IORef (NetDev, UTCTime)

-- The more information available, the better.
-- Note that names don't matter. Therefore, if only the names differ,
-- a compare evaluates to EQ while (==) evaluates to False.
instance Ord NetDev where
    compare NA NA              = EQ
    compare NA _               = LT
    compare _  NA              = GT
    compare (NI _) (NI _)      = EQ
    compare (NI _) (ND _ _ _)  = LT
    compare (ND _ _ _) (NI _)  = GT
    compare (ND _ x1 y1) (ND _ x2 y2) =
        if downcmp /= EQ
           then downcmp
           else y1 `compare` y2
      where downcmp = x1 `compare` x2

netConfig :: IO MConfig
netConfig = mkMConfig
    "<dev>: <rx>KB|<tx>KB"      -- template
    ["dev", "rx", "tx", "rxbar", "txbar"]     -- available replacements

operstateDir :: String -> FilePath
operstateDir d = "/sys/class/net" </> d </> "operstate"

existingDevs :: IO [String]
existingDevs = getDirectoryContents "/sys/class/net" >>= filterM isDev
  where isDev d | d `elem` excludes = return False
                | otherwise = doesFileExist (operstateDir d)
        excludes = [".", "..", "lo"]

isUp :: String -> IO Bool
isUp d = do
  operstate <- B.readFile (operstateDir d)
  return $ "up" == (B.unpack . head . B.lines) operstate

readNetDev :: [String] -> IO NetDev
readNetDev (d:x:y:_) = do
  up <- isUp d
  return (if up then ND d (r x) (r y) else NI d)
    where r s | s == "" = 0
              | otherwise = read s / 1024

readNetDev _ = return NA

netParser :: B.ByteString -> IO [NetDev]
netParser = mapM (readNetDev . splitDevLine) . readDevLines
  where readDevLines = drop 2 . B.lines
        splitDevLine = selectCols . wordsBy (`elem` " :") . B.unpack
        selectCols cols = map (cols!!) [0,1,9]
        wordsBy f s = case dropWhile f s of
          [] -> []
          s' -> w : wordsBy f s'' where (w, s'') = break f s'

findNetDev :: String -> IO NetDev
findNetDev dev = do
  nds <- B.readFile "/proc/net/dev" >>= netParser
  case filter isDev nds of
    x:_ -> return x
    _ -> return NA
  where isDev (ND d _ _) = d == dev
        isDev (NI d) = d == dev
        isDev NA = False

formatNet :: Float -> Monitor (String, String)
formatNet d = do
    s <- getConfigValue useSuffix
    dd <- getConfigValue decDigits
    let str = if s then (++"Kb/s") . showDigits dd else showDigits dd
    b <- showLogBar 0.9 d
    x <- showWithColors str d
    return (x, b)

printNet :: NetDev -> Monitor String
printNet nd =
  case nd of
    ND d r t -> do
        (rx, rb) <- formatNet r
        (tx, tb) <- formatNet t
        parseTemplate [d,rx,tx,rb,tb]
    NI _ -> return ""
    NA -> return "N/A"

parseNet :: NetDevRef -> String -> IO NetDev
parseNet nref nd = do
  (n0, t0) <- readIORef nref
  n1 <- findNetDev nd
  t1 <- getCurrentTime
  writeIORef nref (n1, t1)
  let scx = realToFrac (diffUTCTime t1 t0)
      scx' = if scx > 0 then scx else 1
      rate da db = takeDigits 2 $ (db - da) / scx'
      diffRate (ND d ra ta) (ND _ rb tb) = ND d (rate ra rb) (rate ta tb)
      diffRate (NI d) _ = NI d
      diffRate _ (NI d) = NI d
      diffRate _ _ = NA
  return $ diffRate n0 n1

runNet :: NetDevRef -> String -> [String] -> Monitor String
runNet nref i _ = io (parseNet nref i) >>= printNet

parseNets :: [(NetDevRef, String)] -> IO [NetDev]
parseNets = mapM $ \(ref, i) -> parseNet ref i

runNets :: [(NetDevRef, String)] -> [String] -> Monitor String
runNets refs _ = io (parseActive refs) >>= printNet
    where parseActive refs' = parseNets refs' >>= return . selectActive
          selectActive = maximum

startNet :: String -> [String] -> Int -> (String -> IO ()) -> IO ()
startNet i a r cb = do
  t0 <- getCurrentTime
  nref <- newIORef (NA, t0)
  _ <- parseNet nref i
  runM a netConfig (runNet nref i) r cb

startDynNet :: [String] -> Int -> (String -> IO ()) -> IO ()
startDynNet a r cb = do
  devs <- existingDevs
  refs <- forM devs $ \d -> do
            t <- getCurrentTime
            nref <- newIORef (NA, t)
            _ <- parseNet nref d
            return (nref, d)
  runM a netConfig (runNets refs) r cb
