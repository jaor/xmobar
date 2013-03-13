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
    , createWin
    -- * Printing
    -- $print
    , drawInWin, printStrings
    ) where

import Prelude hiding (lookup)
import Graphics.X11.Xlib hiding (textExtents, textWidth)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama
import Graphics.X11.Xrandr

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (handle, SomeException(..))
import Data.Bits
import Data.Map hiding (foldr, map, filter)
import Data.Maybe (fromJust)

import Bitmap
import Config
import Parsers
import Commands
import Actions
import Runnable
import Signal
import Window
import XUtil
import ColorCache

#ifdef XFT
import Graphics.X11.Xft
#endif

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
          , iconS   :: Map FilePath Bitmap
          , config  :: Config
          }

-- | Runs the ReaderT
runX :: XConf -> X () -> IO ()
runX xc f = runReaderT f xc

-- | Starts the main event loop and threads
startLoop :: XConf -> TMVar SignalType -> [[(Maybe ThreadId, TVar String)]] -> IO ()
startLoop xcfg@(XConf _ _ w _ _ _) sig vs = do
#ifdef XFT
    xftInitFtLibrary
#endif
    tv <- atomically $ newTVar []
    _ <- forkIO (handle (handler "checker") (checker tv [] vs sig))
#ifdef THREADED_RUNTIME
    _ <- forkOS (handle (handler "eventer") (eventer sig))
#else
    _ <- forkIO (handle (handler "eventer") (eventer sig))
#endif
#ifdef DBUS
    runIPC sig
#endif
    eventLoop tv xcfg [] sig
  where
    handler thing (SomeException _) =
      void $ putStrLn ("Thread " ++ thing ++ " failed")
    -- Reacts on events from X
    eventer signal =
      allocaXEvent $ \e -> do
        dpy <- openDisplay ""
        xrrSelectInput    dpy (defaultRootWindow dpy) rrScreenChangeNotifyMask
        selectInput       dpy w (exposureMask .|. structureNotifyMask .|. buttonPressMask)

        forever $ do
#ifdef THREADED_RUNTIME
          nextEvent dpy e
#else
          nextEvent' dpy e
#endif
          ev <- getEvent e
          case ev of
            ConfigureEvent {} -> atomically $ putTMVar signal Reposition
            ExposeEvent {} -> atomically $ putTMVar signal Wakeup
            RRScreenChangeNotifyEvent {} -> atomically $ putTMVar signal Reposition
            ButtonEvent {} -> atomically $ putTMVar signal (Action (fi $ ev_x ev))
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
eventLoop :: TVar [String] -> XConf -> [(Action, Position, Position)] -> TMVar SignalType -> IO ()
eventLoop tv xc@(XConf d r w fs is cfg) as signal = do
      typ <- atomically $ takeTMVar signal
      case typ of
         Wakeup -> do
            str <- updateString cfg tv
            xc' <- updateCache d w is str >>= \c -> return xc { iconS = c }
            as' <- updateActions xc r str
            runX xc' $ drawInWin r str
            eventLoop tv xc' as' signal

         Reposition ->
            reposWindow cfg

         ChangeScreen -> do
            ncfg <- updateConfigPosition cfg
            reposWindow ncfg

         Hide   t -> hide   (t*100*1000)
         Reveal t -> reveal (t*100*1000)
         Toggle t -> toggle t

         TogglePersistent -> eventLoop
            tv xc { config = cfg { persistent = not $ persistent cfg } } as signal

         Action x -> action x

    where
        isPersistent = not $ persistent cfg

        hide t
            | t == 0 =
                when isPersistent (hideWindow d w) >> eventLoop tv xc as signal
            | otherwise = do
                void $ forkIO
                     $ threadDelay t >> atomically (putTMVar signal $ Hide 0)
                eventLoop tv xc as signal

        reveal t
            | t == 0 = do
                when isPersistent (showWindow r cfg d w)
                eventLoop tv xc as signal
            | otherwise = do
                void $ forkIO
                     $ threadDelay t >> atomically (putTMVar signal $ Reveal 0)
                eventLoop tv xc as signal

        toggle t = do
            ismapped <- isMapped d w
            atomically (putTMVar signal $ if ismapped then Hide t else Reveal t)
            eventLoop tv xc as signal

        reposWindow rcfg = do
          r' <- repositionWin d w fs rcfg
          eventLoop tv (XConf d r' w fs is rcfg) as signal

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

        action x = do mapM_ (\(a,_,_) -> runAction a) $ filter (\(_, from, to) -> x >= from && x <= to) as
                      eventLoop tv xc as signal

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

updateString :: Config -> TVar [String] -> IO [[(Widget, String, Maybe Action)]]
updateString conf v = do
  s <- atomically $ readTVar v
  let l:c:r:_ = s ++ repeat ""
  io $ mapM (parseString conf) [l, c, r]

updateActions :: XConf -> Rectangle -> [[(Widget, String, Maybe Action)]] ->
                 IO [(Action, Position, Position)]
updateActions conf (Rectangle _ _ wid _) ~[left,center,right] = do
  let (d,fs) = (display &&& fontS) conf
      strLn :: [(Widget, String, Maybe Action)] -> IO [(Maybe Action, Position, Position)]
      strLn  = io . mapM getCoords
      iconW i = maybe 0 Bitmap.width (lookup i $ iconS conf)
      getCoords (Text s,_,a) = textWidth d fs s >>= \tw -> return (a, 0, fi tw)
      getCoords (Icon s,_,a) = return (a, 0, fi $ iconW s)
      partCoord off xs = map (\(a, x, x') -> (fromJust a, x, x')) $
                         filter (\(a, _,_) -> a /= Nothing) $
                         scanl (\(_,_,x') (a,_,w') -> (a, x', x' + w')) (Nothing, 0, off) xs

      totSLen              = foldr (\(_,_,len) -> (+) len) 0
      remWidth xs          = fi wid - totSLen xs
      offs                 = 1
      offset a xs          = case a of
                               C -> (remWidth xs + offs) `div` 2
                               R -> remWidth xs
                               L -> offs

  fmap concat $ mapM (\(a,xs) -> fmap (\xs' -> partCoord (offset a xs') xs') $ strLn xs) $
                zip [L,C,R] [left,center,right]

-- $print

-- | Draws in and updates the window
drawInWin :: Rectangle -> [[(Widget, String, Maybe Action)]] -> X ()
drawInWin (Rectangle _ _ wid ht) ~[left,center,right] = do
  r <- ask
  let (c,d ) = (config &&& display) r
      (w,fs) = (window &&& fontS  ) r
      strLn  = io . mapM getWidth
      iconW i = maybe 0 Bitmap.width (lookup i $ iconS r)
      getWidth (Text s,cl,_) = textWidth d fs s >>= \tw -> return (Text s,cl,fi tw)
      getWidth (Icon s,cl,_) = return (Icon s,cl,fi $ iconW s)

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
             -> Align -> [(Widget, String, Position)] -> X ()
printStrings _ _ _ _ _ [] = return ()
printStrings dr gc fontst offs a sl@((s,c,l):xs) = do
  r <- ask
  (as,ds) <- case s of
               Text t -> io $ textExtents fontst t
               Icon _ -> return (0, 0)
  let (conf,d)             = (config &&& display) r
      Rectangle _ _ wid ht = rect r
      totSLen              = foldr (\(_,_,len) -> (+) len) 0 sl
      valign               = -1 + (fi ht + fi (as + ds)) `div` 2
      remWidth             = fi wid - fi totSLen
      offset               = case a of
                               C -> (remWidth + offs) `div` 2
                               R -> remWidth
                               L -> offs
      (fc,bc)              = case break (==',') c of
                               (f,',':b) -> (f, b           )
                               (f,    _) -> (f, bgColor conf)
  case s of
    (Text t) -> io $ printString d dr fontst gc fc bc offset valign t
    (Icon p) -> io $ maybe (return ()) (drawBitmap d dr gc fc bc offset valign) (lookup p (iconS r))
  printStrings dr gc fontst (offs + l) a xs
