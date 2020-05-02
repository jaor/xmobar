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
-- A plugin for reading from `stdin`.
--
-- Exports:
-- - `StdinReader` to safely display stdin content (striping actions).
-- - `UnsafeStdinReader` to display stdin content as-is.
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.StdinReader (StdinReader(..)) where

import Prelude
import System.Posix.Process
import System.Exit
import System.IO
import Xmobar.Run.Exec
import Xmobar.X11.Actions (stripActions)
import Xmobar.System.Utils (onSomeException)

data StdinReader = StdinReader | UnsafeStdinReader
  deriving (Read, Show)

instance Exec StdinReader where
  start stdinReader cb = do
    s <-
      getLine `onSomeException`
      (\e -> do
         let errorMessage = "xmobar: Received exception " <> show e
         hPrint stderr errorMessage
         cb errorMessage)
    cb $ escape stdinReader s
    eof <- isEOF
    if eof
      then exitImmediately ExitSuccess
      else start stdinReader cb

escape :: StdinReader -> String -> String
escape StdinReader = stripActions
escape UnsafeStdinReader = id
