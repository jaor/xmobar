{-# LANGUAGE DeriveDataTypeable, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A status bar for the Xmonad Window Manager
--
-----------------------------------------------------------------------------

module Xmobar
    ( -- * Main Stuff
      -- $main
      X , XConf (..), runX
    , startLoop
    -- * Program Execution
    -- $command
    , startCommand
    -- * Window Management
    -- $window
    , createWin, updateWin
    -- * Printing
    -- $print
    , drawInWin, printStrings
    ) where

import Prelude hiding (catch)
import Graphics.X11.Xlib hiding (textExtents, textWidth)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama
import Graphics.X11.Xrandr

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (catch, SomeException(..))
import Data.Bits

import Config
import Parsers
import Commands
import Runnable
import Signal
import Window
import XUtil

#ifdef DBUS
import IPC.DBus
#endif

-- $main
--
-- The Xmobar data type and basic loops and functions.

-- | The X type is a ReaderT
type X = ReaderT XConf IO

-- | The ReaderT inner component
data XConf =
    XConf { display :: Display
          , rect    :: Rectangle
          , window  :: Window
          , fontS   :: XFont
          , config  :: Config
          }

-- | Runs the ReaderT
runX :: XConf -> X () -> IO ()
runX xc f = runReaderT f xc

-- | Starts the main event loop and threads
startLoop :: XConf -> TMVar SignalType -> [[(Maybe ThreadId, TVar String)]] -> IO ()
startLoop xcfg@(XConf _ _ w _ _) sig vs = do
    tv <- atomically $ newTVar []
    _ <- forkIO (checker tv [] vs sig `catch`
                   \(SomeException _) -> void (putStrLn "Thread checker failed"))
#ifdef THREADED_RUNTIME
    _ <- forkOS (eventer sig `catch`
#else
    _ <- forkIO (eventer sig `catch`
#endif
                   \(SomeException _) -> void (putStrLn "Thread eventer failed"))
#ifdef DBUS
    runIPC sig
#endif
    eventLoop tv xcfg sig
  where
    -- Reacts on events from X
    eventer signal =
      allocaXEvent $ \e -> do

        dpy <- openDisplay ""
        xrrSelectInput    dpy (defaultRootWindow dpy) rrScreenChangeNotifyMask
        selectInput       dpy w (exposureMask .|. structureNotifyMask)

        forever $ do
#ifdef THREADED_RUNTIME
          nextEvent dpy e
#else
          nextEvent' dpy e
#endif
          ev <- getEvent e
          case ev of
            -- ConfigureEvent {} -> (atomically $ putTMVar signal Reposition)
            ExposeEvent {} -> atomically $ putTMVar signal Wakeup
            RRScreenChangeNotifyEvent {} -> atomically $ putTMVar signal Reposition
            _ -> return ()

-- | Send signal to eventLoop every time a var is updated
checker :: TVar [String]
           -> [String]
           -> [[(Maybe ThreadId, TVar String)]]
           -> TMVar SignalType
           -> IO ()
checker tvar ov vs signal = do
      nval <- atomically $ do
              nv <- mapM concatV vs
              guard (nv /= ov)
              writeTVar tvar nv
              return nv
      atomically $ putTMVar signal Wakeup
      checker tvar nval vs signal
    where
      concatV = fmap concat . mapM (readTVar . snd)


-- | Continuously wait for a signal from a thread or a interrupt handler
eventLoop :: TVar [String] -> XConf -> TMVar SignalType -> IO ()
eventLoop tv xc@(XConf d r w fs cfg) signal = do
      typ <- atomically $ takeTMVar signal
      case typ of
         Wakeup -> do
            runX xc (updateWin tv)
            eventLoop tv xc signal

         Reposition ->
            reposWindow cfg

         ChangeScreen -> do
            ncfg <- updateConfigPosition cfg
            reposWindow ncfg

         Hide   t -> hide   (t*100*1000)
         Reveal t -> reveal (t*100*1000)
         Toggle t -> toggle t

         TogglePersistent -> eventLoop
            tv xc { config = cfg { persistent = not $ persistent cfg } } signal

    where
        isPersistent = not $ persistent cfg

        hide t
            | t == 0 =
                when isPersistent (hideWindow d w) >> eventLoop tv xc signal
            | otherwise = do
                void $ forkIO
                     $ threadDelay t >> atomically (putTMVar signal $ Hide 0)
                eventLoop tv xc signal

        reveal t
            | t == 0 = do
                when isPersistent (showWindow r cfg d w)
                eventLoop tv xc signal
            | otherwise = do
                void $ forkIO
                     $ threadDelay t >> atomically (putTMVar signal $ Reveal 0)
                eventLoop tv xc signal

        toggle t = do
            ismapped <- isMapped d w
            atomically (putTMVar signal $ if ismapped then Hide t else Reveal t)
            eventLoop tv xc signal

        reposWindow rcfg = do
          r' <- repositionWin d w fs rcfg
          eventLoop tv (XConf d r' w fs rcfg) signal

        updateConfigPosition ocfg =
          case position ocfg of
            OnScreen n o -> do
              srs <- getScreenInfo d
              if n == length srs then
                  return (ocfg {position = OnScreen 1 o})
                else
                  return (ocfg {position = OnScreen (n+1) o})
            o ->
              return (ocfg {position = OnScreen 1 o})

-- $command

-- | Runs a command as an independent thread and returns its thread id
-- and the TVar the command will be writing to.
startCommand :: TMVar SignalType
             -> (Runnable,String,String)
             -> IO (Maybe ThreadId, TVar String)
startCommand sig (com,s,ss)
    | alias com == "" = do var <- atomically $ newTVar is
                           atomically $ writeTVar var (s ++ ss)
                           return (Nothing,var)
    | otherwise       = do var <- atomically $ newTVar is
                           let cb str = atomically $ writeTVar var (s ++ str ++ ss)
                           h <- forkIO $ start com cb
                           _ <- forkIO $ trigger com
                                       $ maybe (return ()) (atomically . putTMVar sig)
                           return (Just h,var)
    where is = s ++ "Updating..." ++ ss

updateWin :: TVar [String] -> X ()
updateWin v = do
  xc <- ask
  s <- io $ atomically $ readTVar v
  let (conf,rec) = (config &&& rect) xc
      l:c:r:_ = s ++ repeat ""
  ps <- io $ mapM (parseString conf) [l, c, r]
  drawInWin rec ps

-- $print

-- | Draws in and updates the window
drawInWin :: Rectangle -> [[(String, String)]] -> X ()
drawInWin (Rectangle _ _ wid ht) ~[left,center,right] = do
  r <- ask
  let (c,d ) = (config &&& display) r
      (w,fs) = (window &&& fontS  ) r
      strLn  = io . mapM (\(s,cl) -> textWidth d fs s >>= \tw -> return (s,cl,fi tw))
  withColors d [bgColor c, borderColor c] $ \[bgcolor, bdcolor] -> do
    gc <- io $ createGC  d w
    -- create a pixmap to write to and fill it with a rectangle
    p <- io $ createPixmap d w wid ht
         (defaultDepthOfScreen (defaultScreenOfDisplay d))
    -- the fgcolor of the rectangle will be the bgcolor of the window
    io $ setForeground d gc bgcolor
    io $ fillRectangle d p gc 0 0 wid ht
    -- write to the pixmap the new string
    printStrings p gc fs 1 L =<< strLn left
    printStrings p gc fs 1 R =<< strLn right
    printStrings p gc fs 1 C =<< strLn center
    -- draw 1 pixel border if requested
    io $ drawBorder (border c) d p gc bdcolor wid ht
    -- copy the pixmap with the new string to the window
    io $ copyArea   d p w gc 0 0 wid ht 0 0
    -- free up everything (we do not want to leak memory!)
    io $ freeGC     d gc
    io $ freePixmap d p
    -- resync
    io $ sync       d True

-- | An easy way to print the stuff we need to print
printStrings :: Drawable -> GC -> XFont -> Position
             -> Align -> [(String, String, Position)] -> X ()
printStrings _ _ _ _ _ [] = return ()
printStrings dr gc fontst offs a sl@((s,c,l):xs) = do
  r <- ask
  (as,ds) <- io $ textExtents fontst s
  let (conf,d)             = (config &&& display) r
      Rectangle _ _ wid ht = rect r
      totSLen              = foldr (\(_,_,len) -> (+) len) 0 sl
      valign               = (fi ht `div` 2) + (fi (as + ds) `div` 3)
      remWidth             = fi wid - fi totSLen
      offset               = case a of
                               C -> (remWidth + offs) `div` 2
                               R -> remWidth
                               L -> offs
      (fc,bc)              = case break (==',') c of
                               (f,',':b) -> (f, b           )
                               (f,    _) -> (f, bgColor conf)
  withColors d [bc] $ \[bc'] -> do
    io $ setForeground d gc bc'
    io $ fillRectangle d dr gc offset 0 (fi l) ht
  io $ printString d dr fontst gc fc bc offset valign s
  printStrings dr gc fontst (offs + l) a xs
