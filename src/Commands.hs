-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Commands
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The 'Exec' class and the 'Command' data type.
--
-- The 'Exec' class rappresents the executable types, whose constructors may
-- appear in the 'Config.commands' field of the 'Config.Config' data type.
--
-- The 'Command' data type is for OS commands to be run by xmobar
--
-----------------------------------------------------------------------------

module Commands
    ( Command (..)
    , Exec    (..)
    , tenthSeconds
    ) where

import Prelude
import Control.Concurrent
import Control.Exception (handle, SomeException(..))
import Data.Char
import System.Process
import System.Exit
import System.IO (hClose)

import Signal
import XUtil

class Show e => Exec e where
    alias   :: e -> String
    alias   e    = takeWhile (not . isSpace) $ show e
    rate    :: e -> Int
    rate    _    = 10
    run     :: e -> IO String
    run     _    = return ""
    start   :: e -> (String -> IO ()) -> IO ()
    start   e cb = go
        where go = run e >>= cb >> tenthSeconds (rate e) >> go
    trigger :: e -> (Maybe SignalType -> IO ()) -> IO ()
    trigger _ sh  = sh Nothing

data Command = Com Program Args Alias Rate
               deriving (Show,Read,Eq)

type Args    = [String]
type Program = String
type Alias   = String
type Rate    = Int

instance Exec Command where
    alias (Com p    _    a _)
        | p /= ""             = if a == "" then p else a
        | otherwise           = ""
    start (Com prog args _ r) cb = if r > 0 then go else exec
        where go = exec >> tenthSeconds r >> go
              exec = do
                (i,o,e,p) <- runInteractiveProcess prog args Nothing Nothing
                exit <- waitForProcess p
                let closeHandles = hClose o >> hClose i >> hClose e
                    getL = handle (\(SomeException _) -> return "")
                                  (hGetLineSafe o)
                case exit of
                  ExitSuccess -> do str <- getL
                                    closeHandles
                                    cb str
                  _ -> do closeHandles
                          cb $ "Could not execute command " ++ prog


-- | Work around to the Int max bound: since threadDelay takes an Int, it
-- is not possible to set a thread delay grater than about 45 minutes.
-- With a little recursion we solve the problem.
tenthSeconds :: Int -> IO ()
tenthSeconds s | s >= x = do threadDelay (x * 100000)
                             tenthSeconds (s - x)
               | otherwise = threadDelay (s * 100000)
               where x = (maxBound :: Int) `div` 100000
