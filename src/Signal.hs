{-# LANGUAGE DeriveDataTypeable, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Signal
-- Copyright   :  (c) Andrea Rosatto
--             :  (c) Jose A. Ortega Ruiz
--             :  (c) Jochen Keil
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Signal handling, including DBUS when available
--
-----------------------------------------------------------------------------

module Signal where

import Data.Typeable (Typeable)
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import System.Posix.Signals
import Graphics.X11.Xlib.Types (Position)

#ifdef DBUS
import DBus (IsVariant(..))
import Control.Monad ((>=>))
#endif

import Plugins.Utils (safeHead)

data WakeUp = WakeUp deriving (Show,Typeable)
instance Exception WakeUp

data SignalType = Wakeup
                | Reposition
                | ChangeScreen
                | Hide   Int
                | Reveal Int
                | Toggle Int
                | TogglePersistent
                | Action Position
    deriving (Read, Show)

#ifdef DBUS
instance IsVariant SignalType where
    toVariant   = toVariant . show
    fromVariant = fromVariant >=> parseSignalType
#endif

parseSignalType :: String -> Maybe SignalType
parseSignalType = fmap fst . safeHead . reads

-- | Signal handling
setupSignalHandler :: IO (TMVar SignalType)
setupSignalHandler = do
   tid   <- newEmptyTMVarIO
   installHandler sigUSR2 (Catch $ updatePosHandler tid) Nothing
   installHandler sigUSR1 (Catch $ changeScreenHandler tid) Nothing
   return tid

updatePosHandler :: TMVar SignalType -> IO ()
updatePosHandler sig = do
   atomically $ putTMVar sig Reposition
   return ()

changeScreenHandler :: TMVar SignalType -> IO ()
changeScreenHandler sig = do
   atomically $ putTMVar sig ChangeScreen
   return ()
