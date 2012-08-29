-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.PipeReader2
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for reading from named pipes. As opposed to PipeReader, this
-- plugin displays a default string when it starts.
--
-----------------------------------------------------------------------------

module Plugins.PipeReader2 where

import System.IO
import Plugins

data PipeReader2 = PipeReader2 String String String
    deriving (Read, Show)

instance Exec PipeReader2 where
    alias (PipeReader2 _ a _)    = a
    start (PipeReader2 p _ def) cb = do
        h <- openFile p ReadWriteMode
        cb def
        forever (hGetLineSafe h >>= cb)
        where forever a = a >> forever a
