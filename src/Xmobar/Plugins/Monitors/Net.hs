-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Net
-- Copyright   :  (c) 2011, 2012, 2013, 2014, 2017 Jose Antonio Ortega Ruiz
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

module Xmobar.Plugins.Monitors.Net (
                        startNet
                      , startDynNet
                      ) where

import Xmobar.Plugins.Monitors.Common

import Data.Word (Word64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Control.Monad (forM, filterM)
import System.Directory (getDirectoryContents, doesFileExist)
import System.FilePath ((</>))
import System.Console.GetOpt
import System.IO.Error (catchIOError)

import qualified Data.ByteString.Lazy.Char8 as B

type DevList = [String]

parseDevList :: String -> DevList
parseDevList = splitOnComma
  where splitOnComma [] = [[]]
        splitOnComma (',':xs) = [] : splitOnComma xs
        splitOnComma (x:xs) =
           let rest = splitOnComma xs
           in (x : head rest) : tail rest

data NetOpts = NetOpts
  { rxIconPattern :: Maybe IconPattern
  , txIconPattern :: Maybe IconPattern
  , onlyDevList :: Maybe DevList
  }

defaultOpts :: NetOpts
defaultOpts = NetOpts
  { rxIconPattern = Nothing
  , txIconPattern = Nothing
  , onlyDevList = Nothing
  }

options :: [OptDescr (NetOpts -> NetOpts)]
options =
  [ Option "" ["rx-icon-pattern"] (ReqArg (\x o ->
     o { rxIconPattern = Just $ parseIconPattern x }) "") ""
  , Option "" ["tx-icon-pattern"] (ReqArg (\x o ->
     o { txIconPattern = Just $ parseIconPattern x }) "") ""
  , Option "" ["devices"] (ReqArg (\x o ->
     o { onlyDevList = Just $ parseDevList x }) "") ""
  ]

parseOpts :: [String] -> IO NetOpts
parseOpts argv =
  case getOpt Permute options argv of
    (o, _, []) -> return $ foldr id defaultOpts o
    (_, _, errs) -> ioError . userError $ concat errs

data UnitPerSec = Bs | KBs | MBs | GBs deriving (Eq,Enum,Ord)
data NetValue = NetValue Float UnitPerSec deriving (Eq,Show)

instance Show UnitPerSec where
    show Bs  = "B/s"
    show KBs = "KB/s"
    show MBs = "MB/s"
    show GBs = "GB/s"

data NetDev num
    = NA
    | NI String
    | ND String num num deriving (Eq,Show,Read)

type NetDevRawTotal = NetDev Word64
type NetDevRate = NetDev Float

type NetDevRef = IORef (NetDevRawTotal, UTCTime)

-- The more information available, the better.
-- Note that names don't matter. Therefore, if only the names differ,
-- a compare evaluates to EQ while (==) evaluates to False.
instance Ord num => Ord (NetDev num) where
    compare NA NA              = EQ
    compare NA _               = LT
    compare _  NA              = GT
    compare (NI _) (NI _)      = EQ
    compare (NI _) ND {}       = LT
    compare ND {} (NI _)     = GT
    compare (ND _ x1 y1) (ND _ x2 y2) =
        if downcmp /= EQ
           then downcmp
           else y1 `compare` y2
      where downcmp = x1 `compare` x2

netConfig :: IO MConfig
netConfig = mkMConfig
    "<dev>: <rx>KB|<tx>KB"      -- template
    ["dev", "rx", "tx", "rxbar", "rxvbar", "rxipat", "txbar", "txvbar", "txipat"]     -- available replacements

operstateDir :: String -> FilePath
operstateDir d = "/sys/class/net" </> d </> "operstate"

existingDevs :: IO [String]
existingDevs = getDirectoryContents "/sys/class/net" >>= filterM isDev
  where isDev d | d `elem` excludes = return False
                | otherwise = doesFileExist (operstateDir d)
        excludes = [".", "..", "lo"]

isUp :: String -> IO Bool
isUp d = flip catchIOError (const $ return False) $ do
  operstate <- B.readFile (operstateDir d)
  return $! (B.unpack . head . B.lines) operstate `elem`  ["up", "unknown"]

readNetDev :: [String] -> IO NetDevRawTotal
readNetDev (d:x:y:_) = do
  up <- isUp d
  return (if up then ND d (r x) (r y) else NI d)
    where r s | s == "" = 0
              | otherwise = read s

readNetDev _ = return NA

netParser :: B.ByteString -> IO [NetDevRawTotal]
netParser = mapM (readNetDev . splitDevLine) . readDevLines
  where readDevLines = drop 2 . B.lines
        splitDevLine = selectCols . wordsBy (`elem` " :") . B.unpack
        selectCols cols = map (cols!!) [0,1,9]
        wordsBy f s = case dropWhile f s of
          [] -> []
          s' -> w : wordsBy f s'' where (w, s'') = break f s'

findNetDev :: String -> IO NetDevRawTotal
findNetDev dev = do
  nds <- B.readFile "/proc/net/dev" >>= netParser
  case filter isDev nds of
    x:_ -> return x
    _ -> return NA
  where isDev (ND d _ _) = d == dev
        isDev (NI d) = d == dev
        isDev NA = False

formatNet :: Maybe IconPattern -> Float -> Monitor (String, String, String, String)
formatNet mipat d = do
    s <- getConfigValue useSuffix
    dd <- getConfigValue decDigits
    let str True v = showDigits dd d' ++ show u
            where (NetValue d' u) = byteNetVal v
        str False v = showDigits dd $ v / 1024
    b <- showLogBar 0.9 d
    vb <- showLogVBar 0.9 d
    ipat <- showLogIconPattern mipat 0.9 d
    x <- showWithColors (str s) d
    return (x, b, vb, ipat)

printNet :: NetOpts -> NetDevRate -> Monitor String
printNet opts nd =
  case nd of
    ND d r t -> do
        (rx, rb, rvb, ripat) <- formatNet (rxIconPattern opts) r
        (tx, tb, tvb, tipat) <- formatNet (txIconPattern opts) t
        parseTemplate [d,rx,tx,rb,rvb,ripat,tb,tvb,tipat]
    NI _ -> return ""
    NA -> getConfigValue naString

parseNet :: NetDevRef -> String -> IO NetDevRate
parseNet nref nd = do
  (n0, t0) <- readIORef nref
  n1 <- findNetDev nd
  t1 <- getCurrentTime
  writeIORef nref (n1, t1)
  let scx = realToFrac (diffUTCTime t1 t0)
      scx' = if scx > 0 then scx else 1
      rate da db = takeDigits 2 $ fromIntegral (db - da) / scx'
      diffRate (ND d ra ta) (ND _ rb tb) = ND d (rate ra rb) (rate ta tb)
      diffRate (NI d) _ = NI d
      diffRate _ (NI d) = NI d
      diffRate _ _ = NA
  return $ diffRate n0 n1

runNet :: NetDevRef -> String -> [String] -> Monitor String
runNet nref i argv = do
  dev <- io $ parseNet nref i
  opts <- io $ parseOpts argv
  printNet opts dev

parseNets :: [(NetDevRef, String)] -> IO [NetDevRate]
parseNets = mapM $ uncurry parseNet

runNets :: [(NetDevRef, String)] -> [String] -> Monitor String
runNets refs argv = do
  opts <- io $ parseOpts argv
  dev <- io $ parseActive $ filterRefs opts refs
  printNet opts dev
    where parseActive refs' = fmap selectActive (parseNets refs')
          refInDevList opts' (_, refname') = case onlyDevList opts' of
            Just theList -> refname' `elem` theList
            Nothing -> True
          filterRefs opts' refs' = case filter (refInDevList opts') refs' of
            [] -> refs'
            xs -> xs
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

byteNetVal :: Float -> NetValue
byteNetVal v
    | v < 1024**1 = NetValue v Bs
    | v < 1024**2 = NetValue (v/1024**1) KBs
    | v < 1024**3 = NetValue (v/1024**2) MBs
    | otherwise   = NetValue (v/1024**3) GBs
