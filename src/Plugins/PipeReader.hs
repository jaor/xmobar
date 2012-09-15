-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.PipeReader
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for reading from named pipes
--
-----------------------------------------------------------------------------

module Plugins.PipeReader where

import System.IO
import Plugins

data PipeReader = PipeReader String String
    deriving (Read, Show)

instance Exec PipeReader where
    alias (PipeReader _ a)    = a
    start (PipeReader p _) cb = do
        let (def, pipe) = split ':' p
        h <- openFile pipe ReadWriteMode
        cb def
        forever (hGetLineSafe h >>= cb)
      where
        forever a = a >> forever a
        split c xs | c `elem` xs = let (pre, post) = span ((/=) c) xs
                                   in (pre, dropWhile ((==) c) post)
                   | otherwise   = ([], xs)
