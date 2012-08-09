{-# LANGUAGE DeriveDataTypeable #-}

module Signal where

import Data.Typeable (Typeable)
import Control.Concurrent
import Control.Exception hiding (handle)
import System.Posix.Signals

data WakeUp = WakeUp deriving (Show,Typeable)
instance Exception WakeUp

data SignalType = Wakeup
                | Reposition
                | ChangeScreen
                | Hide
                | Reveal
                | Toggle
    deriving (Read, Show)

-- | Signal handling
setupSignalHandler :: IO (MVar SignalType)
setupSignalHandler = do
   tid   <- newEmptyMVar
   installHandler sigUSR2 (Catch $ updatePosHandler tid) Nothing
   installHandler sigUSR1 (Catch $ changeScreenHandler tid) Nothing
   return tid

updatePosHandler :: MVar SignalType -> IO ()
updatePosHandler sig = do
   putMVar sig Reposition
   return ()

changeScreenHandler :: MVar SignalType -> IO ()
changeScreenHandler sig = do
   putMVar sig ChangeScreen
   return ()
