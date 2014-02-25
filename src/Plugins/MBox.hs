{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.MBox
-- Copyright   :  (c) Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for checking mail in mbox files.
--
-----------------------------------------------------------------------------

module Plugins.MBox (MBox(..)) where

import Prelude
import Plugins
#ifdef INOTIFY
import Plugins.Utils (changeLoop, expandHome)

import Control.Monad (when)
import Control.Concurrent.STM
import Control.Exception (SomeException (..), handle, evaluate)

import System.Console.GetOpt
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.INotify (Event(..), EventVariety(..), initINotify, addWatch)

import qualified Data.ByteString.Lazy.Char8 as B

data Options = Options
               { oAll :: Bool
               , oUniq :: Bool
               , oDir :: FilePath
               , oPrefix :: String
               , oSuffix :: String
               }

defaults :: Options
defaults = Options {
  oAll = False, oUniq = False, oDir = "", oPrefix = "", oSuffix = ""
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "a" ["all"] (NoArg (\o -> o { oAll = True })) ""
  , Option "u" [] (NoArg (\o -> o { oUniq = True })) ""
  , Option "d" ["dir"] (ReqArg (\x o -> o { oDir = x }) "") ""
  , Option "p" ["prefix"] (ReqArg (\x o -> o { oPrefix = x }) "") ""
  , Option "s" ["suffix"] (ReqArg (\x o -> o { oSuffix = x }) "") ""
  ]

parseOptions :: [String] -> IO Options
parseOptions args =
  case getOpt Permute options args of
    (o, _, []) -> return $ foldr id defaults o
    (_, _, errs) -> ioError . userError $ concat errs

#else
import System.IO
#endif

-- | A list of display names, paths to mbox files and display colours,
-- followed by a list of options.
data MBox = MBox [(String, FilePath, String)] [String] String
          deriving (Read, Show)

instance Exec MBox where
  alias (MBox _ _ a) = a
#ifndef INOTIFY
  start _ _ = do
    hPutStrLn stderr $ "Warning: xmobar is not compiled with -fwith_inotify" ++
          " but the MBox plugin requires it"
#else
  start (MBox boxes args _) cb = do
    opts <- parseOptions args
    let showAll = oAll opts
        prefix = oPrefix opts
        suffix = oSuffix opts
        uniq = oUniq opts
        names = map (\(t, _, _) -> t) boxes
        colors = map (\(_, _, c) -> c) boxes
        extractPath (_, f, _) = expandHome $ oDir opts </> f
        events = [CloseWrite]

    i <- initINotify
    vs <- mapM (\b -> do
                   f <- extractPath b
                   exists <- doesFileExist f
                   n <- if exists then countMails f else return (-1)
                   v <- newTVarIO (f, n)
                   when exists $
                     addWatch i events f (handleNotification v) >> return ()
                   return v)
                boxes

    changeLoop (mapM (fmap snd . readTVar) vs) $ \ns ->
      let s = unwords [ showC uniq m n c | (m, n, c) <- zip3 names ns colors
                                         , showAll || n > 0 ]
      in cb (if null s then "" else prefix ++ s ++ suffix)

showC :: Bool -> String -> Int -> String -> String
showC u m n c =
  if c == "" then msg else "<fc=" ++ c ++ ">" ++ msg ++ "</fc>"
    where msg = m ++ if not u || n > 1 then show n else ""

countMails :: FilePath -> IO Int
countMails f =
  handle (\(SomeException _) -> evaluate 0)
         (do txt <- B.readFile f
             evaluate $! length . filter (B.isPrefixOf from) . B.lines $ txt)
  where from = B.pack "From "

handleNotification :: TVar (FilePath, Int) -> Event -> IO ()
handleNotification v _ =  do
  (p, _) <- atomically $ readTVar v
  n <- countMails p
  atomically $ writeTVar v (p, n)
#endif
