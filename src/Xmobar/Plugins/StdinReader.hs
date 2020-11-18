{-# LANGUAGE ViewPatterns #-}

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
import System.IO.Error (isEOFError)
import Xmobar.Run.Exec
import Xmobar.X11.Actions (stripActions)
import Control.Exception
import Control.Monad (forever)

data StdinReader = StdinReader | UnsafeStdinReader
  deriving (Read, Show)

instance Exec StdinReader where
  start stdinReader cb = forever $ (cb . escape stdinReader =<< getLine) `catch` handler
    where
      -- rethrow async exceptions like ThreadKilled, etc.
      handler (fromException -> Just e) = throwIO (e :: SomeAsyncException)
      -- XMonad.Hooks.DynamicLog.statusBar starts new xmobar on every xmonad
      -- reload and the old xmobar is only signalled to exit via the pipe
      -- being closed, so we must unconditionally terminate on EOF, otherwise
      -- there'd be a pileup of xmobars
      handler (fromException -> Just e) | isEOFError e = exitImmediately ExitSuccess
      -- any other exception, like "invalid argument (invalid byte sequence)",
      -- is logged to both stderr and the bar itself
      handler e = do
        let errorMessage = "xmobar: Received exception " <> show e
        hPutStrLn stderr errorMessage
        cb $ stripActions errorMessage

escape :: StdinReader -> String -> String
escape StdinReader = stripActions
escape UnsafeStdinReader = id
