-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Net
-- Copyright   :  (c) 2011 Jose Antonio Ortega Ruiz
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

module Plugins.Monitors.Net (startNet) where

import Plugins.Monitors.Common

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

import qualified Data.ByteString.Lazy.Char8 as B

data OperState = Up | Down
    deriving (Eq, Show, Read)

data NetDev = NA
            | ND { netDev :: String
                 , netRx :: Float
                 , netTx :: Float
                 , netOperState :: OperState
                 } deriving (Eq,Show,Read)

type NetDevRef = IORef (NetDev, UTCTime)

netConfig :: IO MConfig
netConfig = mkMConfig
    "<dev>: <rx>KB|<tx>KB"      -- template
    ["dev", "rx", "tx", "rxbar", "txbar"]     -- available replacements

-- Given a list of indexes, take the indexed elements from a list.
getNElements :: [Int] -> [a] -> [a]
getNElements ns as = map (as!!) ns

-- Split into words, with word boundaries indicated by the given predicate.
-- Drops delimiters.  Duplicates 'Data.List.Split.wordsBy'.
--
-- > map (wordsBy (`elem` " :")) ["lo:31174097 31174097", "eth0:  43598 88888"]
--
-- will become @[["lo","31174097","31174097"], ["eth0","43598","88888"]]@
wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
    [] -> []
    s' -> w : wordsBy f s'' where (w, s'') = break f s'

readNetDev :: [String] -> NetDev
readNetDev [] = NA
readNetDev xs =
    ND (head xs) (r (xs !! 1)) (r (xs !! 2)) Down
       where r s | s == "" = 0
                 | otherwise = read s / 1024


parseOperState :: String -> OperState
parseOperState state | state == "down" = Down
                     | otherwise       = Up

fillOperState :: NetDev -> IO NetDev
fillOperState (ND d r t _) = do
        operstate <- B.readFile $ "/sys/class/net/" ++ d ++ "/operstate"
        return (ND d r t (parseOperState $ (B.unpack . head . B.lines) operstate))

fillOperState NA = return NA

fileNet :: IO [NetDev]
fileNet = do 
    devs <- netParser `fmap` B.readFile "/proc/net/dev"
    mapM fillOperState devs

findNetDev :: String -> IO NetDev
findNetDev dev = do
  nds <- fileNet
  case filter (\d -> netDev d == dev) nds of
    x:_ -> return x
    _ -> return NA

netParser :: B.ByteString -> [NetDev]
netParser =
    map (readNetDev . getNElements [0,1,9] . wordsBy (`elem` " :") . B.unpack) . drop 2 . B.lines

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
         ND d r t o -> case o of
            Up -> do (rx, rb) <- formatNet r
                     (tx, tb) <- formatNet t
                     parseTemplate [d,rx,tx,rb,tb]
            Down -> return ""
         NA -> return ""

parseNet :: NetDevRef -> String -> IO NetDev
parseNet nref nd = do
  (n0, t0) <- readIORef nref
  n1 <- findNetDev nd
  t1 <- getCurrentTime
  writeIORef nref (n1, t1)
  let scx = realToFrac (diffUTCTime t1 t0)
      scx' = if scx > 0 then scx else 1
      netRate f da db = takeDigits 2 $ (f db - f da) / scx'
      diffRate NA _ = NA
      diffRate _ NA = NA
      diffRate da db = ND nd (netRate netRx da db) (netRate netTx da db) (netOperState db)
  return $ diffRate n0 n1

runNet :: NetDevRef -> String -> [String] -> Monitor String
runNet nref i _ = io (parseNet nref i) >>= printNet

startNet :: String -> [String] -> Int -> (String -> IO ()) -> IO ()
startNet i a r cb = do
  t0 <- getCurrentTime
  nref <- newIORef (NA, t0)
  _ <- parseNet nref i
  runM a netConfig (runNet nref i) r cb
