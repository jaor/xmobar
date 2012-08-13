-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.BufferedPipeReader
-- Copyright   :  (c) Jochen Keil
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jochen Keil <jochen dot keil at gmail dot com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for reading (temporarily) from named pipes with reset
--
-----------------------------------------------------------------------------

module Plugins.BufferedPipeReader where

import Control.Monad(forM_, when)
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import System.IO.Unsafe(unsafePerformIO)

import Plugins
import Signal

data BufferedPipeReader = BufferedPipeReader String [(Int, Bool, String)]
    deriving (Read, Show)

signal :: MVar SignalType
signal = unsafePerformIO newEmptyMVar

instance Exec BufferedPipeReader where
    alias      ( BufferedPipeReader a _  )    = a

    trigger br@( BufferedPipeReader _ _  ) sh =
        takeMVar signal >>= sh . Just >> trigger br sh

    start      ( BufferedPipeReader _ ps ) cb = do

        (chan, str, rst) <- initV
        forM_ ps $ \p -> forkIO $ reader p chan
        writer chan str rst

        where
        initV :: IO ( TChan (Int, Bool, String), TMVar String, TVar Bool )
        initV = atomically $ do
            tc <- newTChan
            ts <- newEmptyTMVar
            tb <- newTVar False
            return (tc, ts, tb)

        reader :: (Int, Bool, FilePath) -> TChan (Int, Bool, String) -> IO ()
        reader p@(to, tg, fp) tc = do
            openFile fp ReadWriteMode >>= hGetLineSafe >>= \dt ->
                atomically $ writeTChan tc (to, tg, dt)
            reader p tc

        writer :: TChan (Int, Bool, String) -> TMVar String -> TVar Bool -> IO ()
        writer tc ts otb = do
            (to, tg, dt, ntb) <- update
            cb dt
            when tg $ putMVar signal Reveal
            when (to /= 0) $ sfork $ reset to tg ts ntb
            writer tc ts ntb

            where
            sfork :: IO () -> IO ()
            sfork f = forkIO f >> return ()

            update :: IO (Int, Bool, String, TVar Bool)
            update = atomically $ do
                (to, tg, dt) <- readTChan tc
                when (to == 0) $ tryPutTMVar ts dt >> return ()
                writeTVar otb False
                tb <- newTVar True
                return (to, tg, dt, tb)

        reset :: Int -> Bool -> TMVar String -> TVar Bool -> IO ()
        reset to tg ts tb = do
            threadDelay ( to * 100 * 1000 )
            readTVarIO tb >>= \b -> when b $ do
                when tg $ putMVar signal Hide
                atomically (tryTakeTMVar ts) >>= maybe (return ()) cb
