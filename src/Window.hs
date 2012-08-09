module Window where

import Prelude hiding (catch)
import Graphics.X11.Xlib hiding (textExtents, textWidth)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama

import Data.Maybe(fromMaybe)
import System.Posix.Process (getProcessID)

import Config
import XUtil

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
    pw i     = rw * min 100 i `div` 100
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
    | otherwise          = [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, xs, xe]
    where st = cy + ch
          sb = rwh - cy
          xs = cx -- a simple calculation for horizontal (x) placement
          xe = xs + cw
getStaticStrutValues _ _ = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

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

hideWindow :: Display -> Window -> IO ()
hideWindow d w = do
    a <- internAtom d "_NET_WM_STRUT_PARTIAL"    False
    c <- internAtom d "CARDINAL"                 False
    changeProperty32 d w a c propModeReplace $ replicate 12 0
    unmapWindow d w
    sync d False

showWindow :: Display -> Window -> IO ()
showWindow d w = mapWindow d w >> sync d False

isMapped :: Display -> Window -> IO Bool
isMapped d w = fmap ism $ getWindowAttributes d w
    where ism (WindowAttributes { wa_map_state = wms }) = wms /= waIsUnmapped
