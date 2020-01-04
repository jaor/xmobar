{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Mail
-- Copyright   :  (c) Spencer Janssen
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <sjanssen@cse.unl.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for checking mail.
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Mail(Mail(..),MailX(..)) where

import Xmobar.Run.Exec
#ifdef INOTIFY

import Xmobar.Plugins.Monitors.Common (parseOptsWith)
import Xmobar.System.Utils (expandHome, changeLoop)

import Control.Monad
import Control.Concurrent.STM

import System.Directory
import System.FilePath
import System.INotify
import System.Console.GetOpt

import Data.List (isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as S

#if MIN_VERSION_hinotify(0,3,10)
import qualified Data.ByteString.Char8 as BS (ByteString, pack, unpack)
unpack :: BS.ByteString -> String
unpack = BS.unpack
pack :: String -> BS.ByteString
pack = BS.pack
#else
unpack :: String -> String
unpack = id
pack :: String -> String
pack = id
#endif
#else
import System.IO
#endif

data MOptions = MOptions
               { oDir :: FilePath
               , oPrefix :: String
               , oSuffix :: String
               }

defaults :: MOptions
defaults = MOptions {oDir = "", oPrefix = "", oSuffix = ""}

options :: [OptDescr (MOptions -> MOptions)]
options =
  [ Option "d" ["dir"] (ReqArg (\x o -> o { oDir = x }) "") ""
  , Option "p" ["prefix"] (ReqArg (\x o -> o { oPrefix = x }) "") ""
  , Option "s" ["suffix"] (ReqArg (\x o -> o { oSuffix = x }) "") ""
  ]

-- | A list of mail box names and paths to maildirs.
data Mail = Mail [(String, FilePath)] String
    deriving (Read, Show)

-- | A list of mail box names, paths to maildirs and display colors.
data MailX = MailX [(String, FilePath, String)] [String] String
    deriving (Read, Show)

instance Exec Mail where
  alias (Mail _ a) = a
  start (Mail ms a) = start (MailX (map (\(n,p) -> (n,p,"")) ms) [] a)

instance Exec MailX where
    alias (MailX _ _ a) = a
#ifndef INOTIFY
    start _ _ =
        hPutStrLn stderr $ "Warning: xmobar is not compiled with -fwith_inotify,"
                        ++ " but the Mail plugin requires it."
#else
    start (MailX ms args _) cb = do
        vs <- mapM (const $ newTVarIO S.empty) ms
        opts <- parseOptsWith options defaults args
        let prefix = oPrefix opts
            suffix = oSuffix opts
            dir = oDir opts
            ps = map (\(_,p,_) -> if null dir then p else dir </> p) ms
            rs = map (</> "new") ps
            ev = [Move, MoveIn, MoveOut, Create, Delete]

        ds <- mapM expandHome rs
        i <- initINotify
        zipWithM_ (\d v -> addWatch i ev d (handle v)) (map pack ds) vs

        forM_ (zip ds vs) $ \(d, v) -> do
            s <- fmap (S.fromList . filter (not . isPrefixOf "."))
                    $ getDirectoryContents d
            atomically $ modifyTVar v (S.union s)

        changeLoop (mapM (fmap S.size . readTVar) vs) $ \ns ->
            let showmbx m n c = if c == ""
                                then m ++ show n
                                else "<fc=" ++ c ++ ">" ++ m ++ show n ++ "</fc>"
                cnts = [showmbx m n c | ((m,_,c), n) <- zip ms ns , n /= 0 ]
            in cb $ if null cnts then "" else prefix ++ unwords cnts ++ suffix

handle :: TVar (Set String) -> Event -> IO ()
handle v e = atomically $ modifyTVar v $ case e of
    Created  {} -> create
    MovedIn  {} -> create
    Deleted  {} -> delete
    MovedOut {} -> delete
    _           -> id
 where
    delete = S.delete ((unpack . filePath) e)
    create = S.insert ((unpack . filePath) e)
#endif
