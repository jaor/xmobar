-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.StdinReader
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for reading from stdin that allows actions
--
-----------------------------------------------------------------------------

module Plugins.NewStdinReader where

import Prelude
import System.Posix.Process
import System.Exit
import System.IO
import Control.Exception (SomeException(..), handle)
import Plugins
import Data.List.Utils (replace) -- package MissingH

data NewStdinReader = NewStdinReader deriving (Read, Show)

instance Exec NewStdinReader where
  start NewStdinReader cb = do
    s <- handle (\(SomeException e) -> do hPrint stderr e; return "")
                (hGetLineSafe stdin)
    cb (unescape s)
    eof <- hIsEOF stdin
    if eof
      then exitImmediately ExitSuccess
      else start NewStdinReader cb

unescape = replace "<<" "<"
