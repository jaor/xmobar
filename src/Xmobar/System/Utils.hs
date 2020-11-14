{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Utils
-- Copyright: (c) 2010, 2018, 2020 Jose Antonio Ortega Ruiz
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


module Xmobar.System.Utils
  ( expandHome
  , changeLoop
  , safeIndex
  ) where

import Control.Monad
import Control.Concurrent.STM
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import System.Environment
import System.FilePath

expandHome :: FilePath -> IO FilePath
expandHome ('~':'/':path) = fmap (</> path) (getEnv "HOME")
expandHome p = return p

changeLoop :: Eq a => STM a -> (a -> IO ()) -> IO ()
changeLoop s f = atomically s >>= go
 where
    go old = do
        f old
        go =<< atomically (do
            new <- s
            guard (new /= old)
            return new)

(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing
{-# INLINE (!!?) #-}

safeIndex :: NE.NonEmpty a -> Int -> a
safeIndex xs index = fromMaybe (NE.head xs) (NE.toList xs !!? index)
