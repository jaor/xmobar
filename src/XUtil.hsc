{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XUtil
-- Copyright   :  (C) 2007, 2011 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unitn.it
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XUtil
    ( XFont
    , initFont
    , initCoreFont
    , initUtf8Font
    , textExtents
    , textWidth
    , printString
    , initColor
    , newWindow
    , nextEvent'
    , readFileSafe
    , hGetLineSafe
    , io
    , fi
    , withColors
    , DynPixel(..)
    ) where

import Control.Concurrent
import Control.Monad.Trans
import Data.IORef
import Foreign
import Foreign.C.Types
import Graphics.X11.Xlib hiding (textExtents, textWidth)
import qualified Graphics.X11.Xlib as Xlib (textExtents, textWidth)
import Graphics.X11.Xlib.Extras
import System.Mem.Weak ( addFinalizer )
import System.Posix.Types (Fd(..))
import System.IO
#if defined XFT || defined UTF8
# if __GLASGOW_HASKELL__ < 612
import qualified System.IO.UTF8 as UTF8 (readFile,hGetLine)
# else
import qualified System.IO as UTF8 (readFile,hGetLine)
# endif
#endif
#if defined XFT
import Data.List
import Graphics.X11.Xft
import Graphics.X11.Xrender
#endif

readFileSafe :: FilePath -> IO String
#if defined XFT || defined UTF8
readFileSafe = UTF8.readFile
#else
readFileSafe = readFile
#endif

hGetLineSafe :: Handle -> IO String
#if defined XFT || defined UTF8
hGetLineSafe = UTF8.hGetLine
#else
hGetLineSafe = hGetLine
#endif

-- Hide the Core Font/Xft switching here
data XFont = Core FontStruct
           | Utf8 FontSet
#ifdef XFT
           | Xft  XftFont
#endif

-- | When initFont gets a font name that starts with 'xft:' it switchs to the Xft backend
-- Example: 'xft:Sans-10'
initFont :: Display ->String -> IO XFont
initFont d s =
#ifdef XFT
       let xftPrefix = "xft:" in
       if  xftPrefix `isPrefixOf` s then
           fmap Xft $ initXftFont d s
       else
#endif
#if defined UTF8 ||  __GLASGOW_HASKELL__ >= 612
           fmap Utf8 $ initUtf8Font d s
#else
           fmap Core $ initCoreFont d s
#endif

-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initCoreFont :: Display -> String -> IO FontStruct
initCoreFont d s = do
  f <- catch getIt fallBack
  addFinalizer f (freeFont d f)
  return f
      where getIt    = loadQueryFont d s
            fallBack = const $ loadQueryFont d "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initUtf8Font :: Display -> String -> IO FontSet
initUtf8Font d s = do
  setupLocale
  (_,_,f) <- catch getIt fallBack
  addFinalizer f (freeFontSet d f)
  return f
      where getIt    = createFontSet d s
            fallBack = const $ createFontSet d "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

#ifdef XFT
initXftFont :: Display -> String -> IO XftFont
initXftFont d s = do
  setupLocale
  f <- xftFontOpen d (defaultScreenOfDisplay d) (drop 4 s)
  addFinalizer f (xftFontClose d f)
  return f
#endif

textWidth :: Display -> XFont -> String -> IO Int
textWidth _   (Utf8 fs) s = return $ fi $ wcTextEscapement fs s
textWidth _   (Core fs) s = return $ fi $ Xlib.textWidth fs s
#ifdef XFT
textWidth dpy (Xft xftdraw) s = do
    gi <- xftTextExtents dpy xftdraw s
    return $ xglyphinfo_xOff gi
#endif

textExtents :: XFont -> String -> IO (Int32,Int32)
textExtents (Core fs) s = do
  let (_,a,d,_) = Xlib.textExtents fs s
  return (a,d)
textExtents (Utf8 fs) s = do
  let (_,rl)  = wcTextExtents fs s
      ascent  = fi $ - (rect_y rl)
      descent = fi $ rect_height rl + (fi $ rect_y rl)
  return (ascent, descent)
#ifdef XFT
textExtents (Xft xftfont) _ = do
  ascent  <- fi `fmap` xftfont_ascent  xftfont
  descent <- fi `fmap` xftfont_descent xftfont
  return (ascent, descent)
#endif

printString :: Display -> Drawable -> XFont -> GC -> String -> String
            -> Position -> Position -> String  -> IO ()
printString d p (Core fs) gc fc bc x y s = do
    setFont d gc $ fontFromFontStruct fs
    withColors d [fc, bc] $ \[fc', bc'] -> do
      setForeground d gc fc'
      setBackground d gc bc'
      drawImageString d p gc x y s

printString d p (Utf8 fs) gc fc bc x y s =
    withColors d [fc, bc] $ \[fc', bc'] -> do
      setForeground d gc fc'
      setBackground d gc bc'
      io $ wcDrawImageString d p fs gc x y s

#ifdef XFT
printString dpy drw fs@(Xft font) gc fc bc x y s = do
  let screen   = defaultScreenOfDisplay dpy
      colormap = defaultColormapOfScreen screen
      visual   = defaultVisualOfScreen screen
  withColors dpy [bc] $ \[bcolor] -> do
    (a,d)  <- textExtents fs s
    gi     <- xftTextExtents dpy font s
    setForeground dpy gc bcolor
    fillRectangle dpy drw gc (x - fi (xglyphinfo_x gi))
                             (y - fi (a + d))
                             (fi $ xglyphinfo_xOff gi)
                             (fi $ 4 + a + d)
    withXftDraw dpy drw visual colormap $
      \draw -> withXftColorName dpy visual colormap fc $
      \color -> xftDrawString draw color font x (y - 2) s
#endif

data DynPixel = DynPixel { allocated :: Bool
                         , pixel     :: Pixel
                         }

-- | Get the Pixel value for a named color: if an invalid name is
-- given the black pixel will be returned.
initColor :: Display -> String -> IO DynPixel
initColor dpy c = (initColor' dpy c) `catch`
                  (const . return $ DynPixel False (blackPixel dpy $ defaultScreen dpy))

type ColorCache = [(String, Color)]
{-# NOINLINE colorCache #-}
colorCache :: IORef ColorCache
colorCache = unsafePerformIO $ newIORef []

getCachedColor :: String -> IO (Maybe Color)
getCachedColor color_name = lookup color_name `fmap` readIORef colorCache

putCachedColor :: String -> Color -> IO ()
putCachedColor name c_id = modifyIORef colorCache $ \c -> (name, c_id) : c

initColor' :: Display -> String -> IO DynPixel
initColor' dpy c = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  cached_color <- getCachedColor c
  c' <- case cached_color of
          Just col -> return col
          _        -> do (c'', _) <- allocNamedColor dpy colormap c
                         putCachedColor c c''
                         return c''
  return $ DynPixel True (color_pixel c')

withColors :: MonadIO m => Display -> [String] -> ([Pixel] -> m a) -> m a
withColors d cs f = do
  ps <- mapM (io . initColor d) cs
  f $ map pixel ps

-- | Creates a window with the attribute override_redirect set to True.
-- Windows Managers should not touch this kind of windows.
newWindow :: Display -> Screen -> Window -> Rectangle -> Bool -> IO Window
newWindow dpy scr rw (Rectangle x y w h) o = do
  let visual   = defaultVisualOfScreen scr
      attrmask = cWOverrideRedirect
  allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes o
           createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr)
                        inputOutput visual attrmask attributes
-- | A version of nextEvent that does not block in foreign calls.
nextEvent' :: Display -> XEventPtr -> IO ()
nextEvent' d p = do
    pend <- pending d
    if pend /= 0
        then nextEvent d p
        else do
            threadWaitRead (Fd fd)
            nextEvent' d p
 where
    fd = connectionNumber d

io :: MonadIO m => IO a -> m a
io = liftIO

-- | Short-hand for 'fromIntegral'
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

#if __GLASGOW_HASKELL__ < 612 && (defined XFT || defined UTF8)
#include <locale.h>
foreign import ccall unsafe "locale.h setlocale"
    setlocale :: CInt -> CString -> IO CString

setupLocale :: IO ()
setupLocale = withCString "" (setlocale $ #const LC_ALL) >> return ()
# else
setupLocale :: IO ()
setupLocale = return ()
#endif
