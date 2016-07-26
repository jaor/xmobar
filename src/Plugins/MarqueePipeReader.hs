-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.MarqueePipeReader
-- Copyright   :  (c) Reto Habluetzel
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for reading from named pipes for long texts with marquee
--
-----------------------------------------------------------------------------

module Plugins.MarqueePipeReader where

import System.IO (openFile, IOMode(ReadWriteMode), Handle)
import Environment
import Plugins (tenthSeconds, Exec(alias, start), hGetLineSafe)
import System.Posix.Files (getFileStatus, isNamedPipe)
import Control.Concurrent(forkIO, threadDelay)
import Control.Concurrent.STM (TChan, atomically, writeTChan, tryReadTChan, newTChan)
import Control.Exception
import Control.Monad(forever, unless)

type Length = Int       -- length of the text to display
type Rate = Int         -- delay in tenth seconds 
type Separator = String -- if text wraps around, use separator

data MarqueePipeReader = MarqueePipeReader String (Length, Rate, Separator) String
    deriving (Read, Show)

instance Exec MarqueePipeReader where
    alias (MarqueePipeReader _ _ a)    = a
    start (MarqueePipeReader p (len, rate, sep) _) cb = do
        (def, pipe) <- split ':' <$> expandEnv p
        unless (null def) (cb def)
        checkPipe pipe
        h <- openFile pipe ReadWriteMode
        line <- hGetLineSafe h
        chan <- atomically newTChan
        forkIO $ writer (toInfTxt line sep) sep len rate chan cb
        forever $ pipeToChan h chan
      where
        split c xs | c `elem` xs = let (pre, post) = span (c /=) xs
                                   in (pre, dropWhile (c ==) post)
                   | otherwise   = ([], xs)

pipeToChan :: Handle -> TChan String -> IO ()
pipeToChan h chan = do
    line <- hGetLineSafe h 
    atomically $ writeTChan chan line

writer :: String -> Separator -> Length -> Rate -> TChan String -> (String -> IO ()) -> IO ()
writer txt sep len rate chan cb = do 
    cb (take len txt)
    mbnext <- atomically $ tryReadTChan chan
    case mbnext of
        Just new -> writer (toInfTxt new sep) sep len rate chan cb
        Nothing -> tenthSeconds rate >> writer (drop 1 txt) sep len rate chan cb 

toInfTxt :: String -> String -> String
toInfTxt line sep = concat (repeat $ line ++ " " ++ sep ++ " ")

checkPipe :: FilePath -> IO ()
checkPipe file = handle (\(SomeException _) -> waitForPipe) $ do
                    status <- getFileStatus file
                    unless (isNamedPipe status) waitForPipe
    where waitForPipe = threadDelay 1000 >> checkPipe file
