------------------------------------------------------------------------------
-- |
-- Module: Plugins.Utils
-- Copyright: (c) 2010 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: Jose A Ortega Ruiz <jao@gnu.org>
-- Stability: unstable
-- Portability: unportable
-- Created: Sat Dec 11, 2010 20:55
--
--
-- Miscellaneous utility functions
--
------------------------------------------------------------------------------


module Plugins.Utils (expandHome, changeLoop, safeHead) where

import Control.Monad
import Control.Concurrent.STM

import System.Environment
import System.FilePath


expandHome :: FilePath -> IO FilePath
expandHome ('~':'/':path) = fmap (</> path) (getEnv "HOME")
expandHome p              = return p

changeLoop :: Eq a => STM a -> (a -> IO ()) -> IO ()
changeLoop s f = atomically s >>= go
 where
    go old = do
        f old
        go =<< atomically (do
            new <- s
            guard (new /= old)
            return new)

safeHead :: [a] -> Maybe a
safeHead    [] = Nothing
safeHead (x:_) = Just x
