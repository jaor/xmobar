------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Plugins.Command
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Dec 02, 2018 05:29
--
--
-- The basic Command plugin
--
------------------------------------------------------------------------------


module Xmobar.Run.Command where

import Control.Exception (handle, SomeException(..))
import System.Process
import System.Exit
import System.IO (hClose, hGetLine, hPrint, stderr)

import Xmobar.Run.Exec

data Command = Com Program Args Alias Rate
             | ComX Program Args String Alias Rate
               deriving (Show,Read,Eq)

type Args    = [String]
type Program = String
type Alias   = String
type Rate    = Int

instance Exec Command where
    alias (ComX p _ _ a _) =
      if p /= "" then (if a == "" then p else a) else ""
    alias (Com p a al r) = alias (ComX p a "" al r)
    start (Com p as al r) cb =
      start (ComX p as ("Could not execute command " ++ p) al r) cb
    start (ComX prog args msg _ r) cb = if r > 0 then go else exec
        where go = doEveryTenthSeconds r exec
              exec = do
                (i,o,e,p) <- runInteractiveProcess prog args Nothing Nothing
                exit <- waitForProcess p
                let errorMessage = "xmobar: Exec exited " <> show exit
                hPrint stderr errorMessage                                   
                let closeHandles = hClose o >> hClose i >> hClose e
                case exit of
                  ExitSuccess -> closeHandles
                  _ -> closeHandles >> cb msg
