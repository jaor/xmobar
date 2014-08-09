{-# LANGUAGE CPP #-}
------------------------------------------------------------------------------
-- |
-- Module: ColorCache
-- Copyright: (c) 2012 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Mon Sep 10, 2012 00:27
--
--
-- Caching X colors
--
------------------------------------------------------------------------------

#if defined XFT

module ColorCache(withColors, withDrawingColors) where

import MinXft

#else
module ColorCache(withColors) where

#endif

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Exception (SomeException, handle)
import Graphics.X11.Xlib

data DynPixel = DynPixel Bool Pixel

initColor :: Display -> String -> IO DynPixel
initColor dpy c = handle black $ initColor' dpy c
  where
    black :: SomeException -> IO DynPixel
    black = const . return $ DynPixel False (blackPixel dpy $ defaultScreen dpy)

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
  ps <- mapM (liftIO . initColor d) cs
  f $ map (\(DynPixel _ pixel) -> pixel) ps

#ifdef XFT

type AXftColorCache = [(String, AXftColor)]
{-# NOINLINE xftColorCache #-}
xftColorCache :: IORef AXftColorCache
xftColorCache = unsafePerformIO $ newIORef []

getXftCachedColor :: String -> IO (Maybe AXftColor)
getXftCachedColor name = lookup name `fmap` readIORef xftColorCache

putXftCachedColor :: String -> AXftColor -> IO ()
putXftCachedColor name cptr =
  modifyIORef xftColorCache $ \c -> (name, cptr) : c

initAXftColor' :: Display -> Visual -> Colormap -> String -> IO AXftColor
initAXftColor' d v cm c = do
  cc <- getXftCachedColor c
  c' <- case cc of
          Just col -> return col
          _        -> do c'' <- mallocAXftColor d v cm c
                         putXftCachedColor c c''
                         return c''
  return c'

initAXftColor :: Display -> Visual -> Colormap -> String -> IO AXftColor
initAXftColor d v cm c = handle black $ (initAXftColor' d v cm c)
  where
    black :: SomeException -> IO AXftColor
    black = (const $ initAXftColor' d v cm "black")

withDrawingColors :: -- MonadIO m =>
                     Display -> Drawable -> String -> String
                    -> (AXftDraw -> AXftColor -> AXftColor -> IO ()) -> IO ()
withDrawingColors dpy drw fc bc f = do
  let screen = defaultScreenOfDisplay dpy
      colormap = defaultColormapOfScreen screen
      visual = defaultVisualOfScreen screen
  fc' <- initAXftColor dpy visual colormap fc
  bc' <- initAXftColor dpy visual colormap bc
  withAXftDraw dpy drw visual colormap $ \draw -> f draw fc' bc'
#endif
