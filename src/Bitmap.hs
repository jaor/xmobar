-----------------------------------------------------------------------------
-- |
-- Module      :  Bitmap
-- Copyright   :  (C) 2013 Alexander Polakov
-- License     :  BSD3
--
-- Maintainer  :  jao@gnu.org
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Bitmap
 ( updateCache
 , drawBitmap
 , Bitmap) where

import Control.Monad
import Data.Map hiding (foldr, map, filter)
import Graphics.X11.Xlib
import System.Directory (doesFileExist)
import System.Mem.Weak ( addFinalizer )
import XGraphic
import ColorCache
import Parsers (Widget(..))

data Bitmap = Bitmap { width  :: Dimension
                     , height :: Dimension
                     , pixmap :: Pixmap
                     }

updateCache :: Display -> Window -> Map FilePath Bitmap -> [[(Widget, String)]]
               -> IO (Map FilePath Bitmap)
updateCache dpy win cache ps = do
  let paths = map (\(Icon p, _) -> p) . concatMap (filter icons) $ ps
      icons (Icon _, _) = True
      icons _ = False
      go m path = if member path m
                     then return m
                     else do bitmap <- loadBitmap dpy win path
                             return $ maybe m (\b -> insert path b m) bitmap
  foldM go cache paths

loadBitmap :: Display -> Drawable -> FilePath -> IO (Maybe Bitmap)
loadBitmap d w p = do
    exist <- doesFileExist p
    if exist
       then do
            (bw, bh, bp, _, _) <- readBitmapFile d w p
            addFinalizer bp (freePixmap d bp)
            return $ Just $ Bitmap bw bh bp
       else
           return Nothing

drawBitmap :: Display -> Drawable -> GC -> String -> String
              -> Position -> Position -> Bitmap -> IO ()
drawBitmap d p gc fc bc x y i =
    withColors d [fc, bc] $ \[fc', bc'] -> do
    let w = width i
        h = height i
    setForeground d gc fc'
    setBackground d gc bc'
    copyPlane d (pixmap i) p gc 0 0 w h x (y - (fromIntegral h)) 1
