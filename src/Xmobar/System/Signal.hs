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

module Xmobar.System.Signal where

import Data.Foldable (for_)
import Data.Typeable (Typeable)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import System.Posix.Signals
import Graphics.X11.Types (Button)
import Graphics.X11.Xlib.Types (Position)
import System.IO

#ifdef DBUS
import DBus (IsVariant(..))
import Control.Monad ((>=>))
#endif

safeHead :: [a] -> Maybe a
safeHead    [] = Nothing
safeHead (x:_) = Just x

data WakeUp = WakeUp deriving (Show,Typeable)
instance Exception WakeUp

data SignalType = Wakeup
                | Reposition
                | ChangeScreen
                | Hide   Int
                | Reveal Int
                | SetAlpha Int
                | Toggle Int
                | TogglePersistent
                | Action Button Position
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


-- | Ensures that the given IO action runs its cleanup actions ('bracket' etc.),
-- even if a signal is caught.
--
-- An exception will be thrown on the thread that called this function when a
-- signal is caught.
withDeferSignals :: IO a -> IO a
withDeferSignals thing = do
  threadId <- myThreadId
  caughtSignal <- newEmptyMVar

  let signals =
        filter (not . flip inSignalSet reservedSignals)
          [ sigQUIT
          , sigTERM
          --, sigINT -- Handler already installed by GHC
          --, sigPIPE -- Handler already installed by GHC
          --, sigUSR1 -- Handled by setupSignalHandler
          --, sigUSR2 -- Handled by setupSignalHandler

          -- One of the following appears to cause instability, see #360
          --, sigHUP
          --, sigILL
          --, sigABRT
          --, sigFPE
          --, sigSEGV
          --, sigALRM
          --, sigBUS
          --, sigPOLL
          --, sigPROF
          --, sigSYS
          --, sigTRAP
          --, sigVTALRM
          --, sigXCPU
          --, sigXFSZ
          ]

  for_ signals $ \s ->

      installHandler s
        (Catch $ do
          tryPutMVar caughtSignal s
          hPutStrLn stderr ("xmobar: Caught signal "++show s++"; exiting...")
          throwTo threadId ThreadKilled)
        Nothing

  thing `finally` do
        s0 <- tryReadMVar caughtSignal
        case s0 of
          Nothing -> pure ()
          Just s -> do
            -- Run the default handler for the signal
            -- hPutStrLn stderr ("xmobar: Running default handler for signal "++show s)
            installHandler s Default Nothing
            raiseSignal s
