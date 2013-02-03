{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XUtil
-- Copyright   :  (C) 2011, 2012 Jose Antonio Ortega Ruiz
--                (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  jao@gnu.org
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
    , newWindow
    , nextEvent'
    , readFileSafe
    , hGetLineSafe
    , io
    , fi
    ) where

import Control.Concurrent
import Control.Monad.Trans
import Control.Exception (SomeException, handle)
import Foreign
-- import Foreign.C.Types
import Data.Foldable (forM_)
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
import MinXft
import Graphics.X11.Xrender
#endif

import ColorCache

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
           | Xft  AXftFont
#endif

-- | When initFont gets a font name that starts with 'xft:' it switchs
-- to the Xft backend Example: 'xft:Sans-10'
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

miscFixedFont :: String
miscFixedFont = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initCoreFont :: Display -> String -> IO FontStruct
initCoreFont d s = do
  f <- handle fallBack getIt
  addFinalizer f (freeFont d f)
  return f
      where getIt = loadQueryFont d s
            fallBack :: SomeException -> IO FontStruct
            fallBack = const $ loadQueryFont d miscFixedFont

-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initUtf8Font :: Display -> String -> IO FontSet
initUtf8Font d s = do
  setupLocale
  (_,_,f) <- handle fallBack getIt
  addFinalizer f (freeFontSet d f)
  return f
      where getIt = createFontSet d s
            fallBack :: SomeException -> IO ([String], String, FontSet)
            fallBack = const $ createFontSet d miscFixedFont

#ifdef XFT
initXftFont :: Display -> String -> IO AXftFont
initXftFont d s = do
  setupLocale
  f <- openAXftFont d (defaultScreenOfDisplay d) (drop 4 s)
  addFinalizer f (closeAXftFont d f)
  return f
#endif

textWidth :: Display -> XFont -> String -> IO Int
textWidth _   (Utf8 fs) s = return $ fi $ wcTextEscapement fs s
textWidth _   (Core fs) s = return $ fi $ Xlib.textWidth fs s
#ifdef XFT
textWidth dpy (Xft xftdraw) s = do
    gi <- xftTxtExtents dpy xftdraw s
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
  ascent  <- fi `fmap` xft_ascent  xftfont
  descent <- fi `fmap` xft_descent xftfont
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
printString dpy drw fs@(Xft font) _ fc bc x y s = do
  (a,d)  <- textExtents fs s
  gi <- xftTxtExtents dpy font s
  withDrawingColors dpy drw fc bc $ \draw -> \fc' -> \bc' ->
    (drawXftRect draw bc' (x + 1 - fi (xglyphinfo_x gi))
                          (y - (a + d) + 1)
                          (xglyphinfo_xOff gi)
                          (a + d)) >>
    (drawXftString draw fc' font x (y - 2) s)
#endif


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
