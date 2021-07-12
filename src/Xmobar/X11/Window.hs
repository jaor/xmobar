-----------------------------------------------------------------------------
-- |
-- Module      :  Window
-- Copyright   :  (c) 2011-18, 20 Jose A. Ortega Ruiz
--             :  (c) 2012 Jochen Keil
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Window manipulation functions
--
-----------------------------------------------------------------------------

module Xmobar.X11.Window where

import Prelude
import Control.Monad (when, unless)
import Graphics.X11.Xlib hiding (textExtents)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama
import Foreign.C.Types (CLong)

import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import System.Posix.Process (getProcessID)

import Xmobar.Config.Types
import Xmobar.X11.Text

-- $window

-- | Creates a window with the attribute override_redirect set to True.
-- Windows Managers should not touch this kind of windows.
newWindow :: Display -> Screen -> Window -> Rectangle -> Bool -> IO Window
newWindow dpy scr rw (Rectangle x y w h) o = do
  let visual = defaultVisualOfScreen scr
      attrmask = if o then cWOverrideRedirect else 0
  allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes o
           createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr)
                        inputOutput visual attrmask attributes

-- | The function to create the initial window
createWin :: Display -> XFont -> Config -> IO (Rectangle,Window)
createWin d fs c = do
  let dflt = defaultScreen d
  srs <- getScreenInfo d
  rootw <- rootWindow d dflt
  (as,ds) <- textExtents fs "0"
  let ht = as + ds + 4
      r = setPosition c (position c) srs (fromIntegral ht)
  win <- newWindow  d (defaultScreenOfDisplay d) rootw r (overrideRedirect c)
  setProperties c d win
  setStruts r c d win srs
  when (lowerOnStart c) $ lowerWindow d win
  unless (hideOnStart c) $ showWindow r c d win
  return (r,win)

-- | Updates the size and position of the window
repositionWin :: Display -> Window -> XFont -> Config -> IO Rectangle
repositionWin d win fs c = do
  srs <- getScreenInfo d
  (as,ds) <- textExtents fs "0"
  let ht = as + ds + 4
      r = setPosition c (position c) srs (fromIntegral ht)
  moveResizeWindow d win (rect_x r) (rect_y r) (rect_width r) (rect_height r)
  setStruts r c d win srs
  sync d False
  return r

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

setPosition :: Config -> XPosition -> [Rectangle] -> Dimension -> Rectangle
setPosition c p rs ht =
  case p' of
    Top -> Rectangle rx ry rw h
    TopP l r -> Rectangle (rx + fi l) ry (rw - fi l - fi r) h
    TopH ch -> Rectangle rx ry rw $ mh ch
    TopW a i -> Rectangle (ax a i) ry (nw i) h
    TopSize a i ch -> Rectangle (ax a i) ry (nw i) (mh ch)
    Bottom -> Rectangle rx ny rw h
    BottomH ch -> Rectangle rx ny rw $ mh ch
    BottomW a i -> Rectangle (ax a i) ny (nw i) h
    BottomP l r -> Rectangle (rx + fi l) ny (rw - fi l - fi r) h
    BottomSize a i ch  -> Rectangle (ax a i) (ny' ch) (nw i) (mh ch)
    Static cx cy cw ch -> Rectangle (fi cx) (fi cy) (fi cw) (fi ch)
    OnScreen _ p'' -> setPosition c p'' [scr] ht
  where
    (scr@(Rectangle rx ry rw rh), p') =
      case p of OnScreen i x -> (fromMaybe (picker rs) $ safeIndex i rs, x)
                _ -> (picker rs, p)
    ny = ry + fi (rh - ht)
    center i = rx + fi (div (remwid i) 2)
    right  i = rx + fi (remwid i)
    remwid i = rw - pw (fi i)
    ax L = const rx
    ax R = right
    ax C = center
    pw i = rw * min 100 i `div` 100
    nw = fi . pw . fi
    h = fi ht
    mh h' = max (fi h') h
    ny' h' = ry + fi (rh - mh h')
    safeIndex i = lookup i . zip [0..]
    picker = if pickBroadest c
             then maximumBy (compare `on` rect_width)
             else head

setProperties :: Config -> Display -> Window -> IO ()
setProperties c d w = do
  let mkatom n = internAtom d n False
  card <- mkatom "CARDINAL"
  atom <- mkatom "ATOM"

  setTextProperty d w (wmClass c) wM_CLASS
  setTextProperty d w (wmName c) wM_NAME

  wtype <- mkatom "_NET_WM_WINDOW_TYPE"
  dock <- mkatom "_NET_WM_WINDOW_TYPE_DOCK"
  changeProperty32 d w wtype atom propModeReplace [fi dock]

  when (allDesktops c) $ do
    desktop <- mkatom "_NET_WM_DESKTOP"
    changeProperty32 d w desktop card propModeReplace [0xffffffff]

  pid  <- mkatom "_NET_WM_PID"
  getProcessID >>= changeProperty32 d w pid card propModeReplace . return . fi

setStruts' :: Display -> Window -> [Foreign.C.Types.CLong] -> IO ()
setStruts' d w svs = do
  let mkatom n = internAtom d n False
  card <- mkatom "CARDINAL"
  pstrut <- mkatom "_NET_WM_STRUT_PARTIAL"
  strut <- mkatom "_NET_WM_STRUT"
  changeProperty32 d w pstrut card propModeReplace svs
  changeProperty32 d w strut card propModeReplace (take 4 svs)

setStruts :: Rectangle -> Config -> Display -> Window -> [Rectangle] -> IO ()
setStruts r c d w rs = do
  let svs = map fi $ getStrutValues r (position c) (getRootWindowHeight rs)
  setStruts' d w svs

getRootWindowHeight :: [Rectangle] -> Int
getRootWindowHeight srs = maximum (map getMaxScreenYCoord srs)
  where
    getMaxScreenYCoord sr = fi (rect_y sr) + fi (rect_height sr)

getStrutValues :: Rectangle -> XPosition -> Int -> [Int]
getStrutValues r@(Rectangle x y w h) p rwh =
  case p of
    OnScreen _ p'   -> getStrutValues r p' rwh
    Top             -> [0, 0, st,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    TopH    _       -> [0, 0, st,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    TopP    _ _     -> [0, 0, st,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    TopW    _ _     -> [0, 0, st,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    TopSize      {} -> [0, 0, st,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    Bottom          -> [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, nx, nw]
    BottomH _       -> [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, nx, nw]
    BottomP _ _     -> [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, nx, nw]
    BottomW _ _     -> [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, nx, nw]
    BottomSize   {} -> [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, nx, nw]
    Static       {} -> getStaticStrutValues p rwh
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
    | otherwise = [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, xs, xe]
    where st = cy + ch
          sb = rwh - cy
          xs = cx -- a simple calculation for horizontal (x) placement
          xe = xs + cw - 1
getStaticStrutValues _ _ = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

drawBorder :: Border -> Int -> Display -> Drawable -> GC -> Pixel
              -> Dimension -> Dimension -> IO ()
drawBorder b lw d p gc c wi ht =  case b of
  NoBorder -> return ()
  TopB       -> drawBorder (TopBM 0) lw d p gc c wi ht
  BottomB    -> drawBorder (BottomBM 0) lw d p gc c wi ht
  FullB      -> drawBorder (FullBM 0) lw d p gc c wi ht
  TopBM m    -> sf >> sla >>
                 drawLine d p gc 0 (fi m + boff) (fi wi) (fi m + boff)
  BottomBM m -> let rw = fi ht - fi m + boff in
                 sf >> sla >> drawLine d p gc 0 rw (fi wi) rw
  FullBM m   -> let mp = fi m
                    pad = 2 * fi mp +  fi lw
                in sf >> sla >>
                     drawRectangle d p gc mp mp (wi - pad) (ht - pad)
  where sf    = setForeground d gc c
        sla   = setLineAttributes d gc (fi lw) lineSolid capNotLast joinMiter
        boff  = borderOffset b lw
--        boff' = calcBorderOffset lw :: Int

hideWindow :: Display -> Window -> IO ()
hideWindow d w = do
    setStruts' d w (replicate 12 0)
    unmapWindow d w >> sync d False

showWindow :: Rectangle -> Config -> Display -> Window -> IO ()
showWindow r c d w = do
    mapWindow d w
    getScreenInfo d >>= setStruts r c d w
    sync d False

isMapped :: Display -> Window -> IO Bool
isMapped d w = ism <$> getWindowAttributes d w
    where ism WindowAttributes { wa_map_state = wms } = wms /= waIsUnmapped

borderOffset :: (Integral a) => Border -> Int -> a
borderOffset b lw =
  case b of
    BottomB    -> negate boffs
    BottomBM _ -> negate boffs
    TopB       -> boffs
    TopBM _    -> boffs
    _          -> 0
  where boffs = calcBorderOffset lw

calcBorderOffset :: (Integral a) => Int -> a
calcBorderOffset = ceiling . (/2) . toDouble
  where toDouble = fi :: (Integral a) => a -> Double
