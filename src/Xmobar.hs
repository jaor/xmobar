{-# LANGUAGE DeriveDataTypeable #-}
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

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Data.Bits
import Data.Maybe(fromMaybe)
import Data.Typeable (Typeable)
import Foreign
import System.Posix.Process (getProcessID)
import System.Posix.Signals

import Config
import Parsers
import Commands
import Runnable
import XUtil

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

data WakeUp = WakeUp deriving (Show,Typeable)
instance Exception WakeUp

data SignalType = Wakeup | Reposition | ChangeScreen

-- | Starts the main event loop and threads
startLoop :: XConf -> [[(Maybe ThreadId, TVar String)]] -> IO ()
startLoop xcfg@(XConf _ _ w _ _) vs = do
    tv <- atomically $ newTVar []
    sig <- setupSignalHandler
    _ <- forkIO (checker tv [] vs sig `catch` \(SomeException _) -> putStrLn "Thread checker failed" >> return ())
    _ <- forkIO (eventer sig `catch` \(SomeException _) -> putStrLn "Thread eventer failed" >> return ())
    eventLoop tv xcfg sig
  where
    -- Reacts on events from X
    eventer signal =
      alloca $ \ptrEventBase ->
      alloca $ \ptrErrorBase ->
      allocaXEvent $ \e -> do

        dpy <- openDisplay ""
        --  keyPressMask is the same value as RRScreenChangeNotifyMask
        xrrSelectInput    dpy (defaultRootWindow dpy) keyPressMask
        selectInput       dpy w (exposureMask .|. structureNotifyMask)

        _ <- xrrQueryExtension dpy ptrEventBase ptrErrorBase
        xrrEventBase <- peek ptrEventBase

        forever $ do
          nextEvent' dpy e
          ev <- getEvent e
          case ev of
            ConfigureEvent {} -> putMVar signal Reposition
            ExposeEvent {} -> putMVar signal Wakeup
            _ ->
              --  0 is the value of RRScreenChangeNotify
              when ( (fromIntegral (ev_event_type ev) - xrrEventBase) == 0)
                   $ putMVar signal Reposition

-- | Send signal to eventLoop every time a var is updated
checker :: TVar [String] -> [String] -> [[(Maybe ThreadId, TVar String)]] -> MVar SignalType -> IO ()
checker tvar ov vs signal = do
      nval <- atomically $ do
              nv <- mapM concatV vs
              guard (nv /= ov)
              writeTVar tvar nv
              return nv
      putMVar signal Wakeup
      checker tvar nval vs signal
    where
      concatV = fmap concat . mapM (readTVar . snd)


-- | Continuously wait for a signal from a thread or a interrupt handler
eventLoop :: TVar [String] -> XConf -> MVar SignalType -> IO ()
eventLoop tv xc@(XConf d _ w fs cfg) signal = do
      typ <- takeMVar signal
      case typ of
         Wakeup -> do
            runX xc (updateWin tv)
            eventLoop tv xc signal

         Reposition ->
            reposWindow cfg

         ChangeScreen -> do
            ncfg <- updateConfigPosition cfg
            reposWindow ncfg

    where
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

-- $command

-- | Runs a command as an independent thread and returns its thread id
-- and the TVar the command will be writing to.
startCommand :: (Runnable,String,String) -> IO (Maybe ThreadId, TVar String)
startCommand (com,s,ss)
    | alias com == "" = do var <- atomically $ newTVar is
                           atomically $ writeTVar var (s ++ ss)
                           return (Nothing,var)
    | otherwise       = do var <- atomically $ newTVar is
                           let cb str = atomically $ writeTVar var (s ++ str ++ ss)
                           h <- forkIO $ start com cb
                           return (Just h,var)
    where is = s ++ "Updating..." ++ ss

-- $window

-- | The function to create the initial window
createWin :: Display -> XFont -> Config -> IO (Rectangle,Window)
createWin d fs c = do
  let dflt = defaultScreen d
  srs     <- getScreenInfo d
  rootw   <- rootWindow d dflt
  (as,ds) <- textExtents fs "0"
  let ht    = as + ds + 4
      (r,o) = setPosition (position c) srs (fi ht)
  win <- newWindow  d (defaultScreenOfDisplay d) rootw r o
  setProperties r c d win srs
  when (lowerOnStart c) (lowerWindow d win)
  mapWindow         d win
  return (r,win)

-- | Updates the size and position of the window
repositionWin :: Display -> Window -> XFont -> Config -> IO Rectangle
repositionWin d win fs c = do
  srs     <- getScreenInfo d
  (as,ds) <- textExtents fs "0"
  let ht    = as + ds + 4
      (r,_) = setPosition (position c) srs (fi ht)
  moveResizeWindow d win (rect_x r) (rect_y r) (rect_width r) (rect_height r)
  setProperties r c d win srs
  return r

setPosition :: XPosition -> [Rectangle] -> Dimension -> (Rectangle,Bool)
setPosition p rs ht =
  case p' of
    Top -> (Rectangle rx ry rw h, True)
    TopW a i -> (Rectangle (ax a i) ry (nw i) h, True)
    TopSize a i ch -> (Rectangle (ax a i) ry (nw i) (mh ch), True)
    Bottom -> (Rectangle rx ny rw h, True)
    BottomW a i -> (Rectangle (ax a i) ny (nw i) h, True)
    BottomSize a i ch  -> (Rectangle (ax a i) (ny' ch) (nw i) (mh ch), True)
    Static cx cy cw ch -> (Rectangle (fi cx) (fi cy) (fi cw) (fi ch), True)
    OnScreen _ p'' -> setPosition p'' [scr] ht
  where
    (scr@(Rectangle rx ry rw rh), p') =
      case p of OnScreen i x -> (fromMaybe (head rs) $ safeIndex i rs, x)
                _ -> (head rs, p)
    ny       = ry + fi (rh - ht)
    center i = rx + fi (div (remwid i) 2)
    right  i = rx + fi (remwid i)
    remwid i = rw - pw (fi i)
    ax L     = const rx
    ax R     = right
    ax C     = center
    pw i     = rw * (min 100 i) `div` 100
    nw       = fi . pw . fi
    h        = fi ht
    mh h'    = max (fi h') h
    ny' h'   = ry + fi (rh - mh h')
    safeIndex i = lookup i . zip [0..]

setProperties :: Rectangle -> Config -> Display -> Window -> [Rectangle] -> IO ()
setProperties r c d w srs = do
  a1 <- internAtom d "_NET_WM_STRUT_PARTIAL"    False
  c1 <- internAtom d "CARDINAL"                 False
  a2 <- internAtom d "_NET_WM_WINDOW_TYPE"      False
  c2 <- internAtom d "ATOM"                     False
  v  <- internAtom d "_NET_WM_WINDOW_TYPE_DOCK" False
  p  <- internAtom d "_NET_WM_PID"              False

  setTextProperty d w "xmobar" wM_CLASS
  setTextProperty d w "xmobar" wM_NAME

  changeProperty32 d w a1 c1 propModeReplace $ map fi $
    getStrutValues r (position c) (getRootWindowHeight srs)
  changeProperty32 d w a2 c2 propModeReplace [fromIntegral v]

  getProcessID >>= changeProperty32 d w p c1 propModeReplace . return . fromIntegral

getRootWindowHeight :: [Rectangle] -> Int
getRootWindowHeight srs = maximum (map getMaxScreenYCoord srs)
  where
    getMaxScreenYCoord sr = fi (rect_y sr) + fi (rect_height sr)

getStrutValues :: Rectangle -> XPosition -> Int -> [Int]
getStrutValues r@(Rectangle x y w h) p rwh =
    case p of
    OnScreen _ p'   -> getStrutValues r p' rwh
    Top             -> [0, 0, st,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    TopW    _ _     -> [0, 0, st,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    TopSize      {} -> [0, 0, st,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    Bottom          -> [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, nx, nw]
    BottomW _ _     -> [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, nx, nw]
    BottomSize   {} -> [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, nx, nw]
    Static _ _ _ _  -> getStaticStrutValues p rwh
    where st = fi y + fi h
          sb = rwh - fi y
          nx = fi x
          nw = fi (x + fi w - 1)

-- get some reaonable strut values for static placement.
getStaticStrutValues :: XPosition -> Int -> [Int]
getStaticStrutValues (Static cx cy cw ch) rwh
    -- if the yPos is in the top half of the screen, then assume a Top
    -- placement, otherwise, it's a Bottom placement
    | cy < (rwh `div` 2) = [0, 0, st,  0, 0, 0, 0, 0, xs, xe,  0,  0]
    | otherwise          = [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, xs, xe]
    where st = cy + ch
          sb = rwh - cy
          xs = cx -- a simple calculation for horizontal (x) placement
          xe = xs + cw
getStaticStrutValues _ _ = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

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

drawBorder :: Border -> Display -> Drawable -> GC -> Pixel
              -> Dimension -> Dimension -> IO ()
drawBorder b d p gc c wi ht =  case b of
  NoBorder -> return ()
  TopB       -> drawBorder (TopBM 0) d p gc c w h
  BottomB    -> drawBorder (BottomBM 0) d p gc c w h
  FullB      -> drawBorder (FullBM 0) d p gc c w h
  TopBM m    -> sf >> drawLine d p gc 0 (fi m) (fi w) 0
  BottomBM m -> let rw = fi h - fi m in
                 sf >> drawLine d p gc 0 rw (fi w) rw
  FullBM m   -> let pad = 2 * fi m; mp = fi m in
                 sf >> drawRectangle d p gc mp mp (w - pad) (h - pad)
  where sf = setForeground d gc c
        (w, h) = (wi - 1, ht - 1)

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
