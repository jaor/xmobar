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
 , Bitmap(..)) where

import Control.Monad
import Data.Map hiding (foldr, map, filter)
import Graphics.X11.Xlib
import System.Directory (doesFileExist)
import System.Mem.Weak ( addFinalizer )
import ColorCache
import Parsers (Widget(..))
import Actions (Action)

data Bitmap = Bitmap { width  :: Dimension
                     , height :: Dimension
                     , pixmap :: Pixmap
                     }

updateCache :: Display -> Window -> Map FilePath Bitmap ->
               [[(Widget, String, Maybe Action)]] -> IO (Map FilePath Bitmap)
updateCache dpy win cache ps = do
  let paths = map (\(Icon p, _, _) -> p) . concatMap (filter icons) $ ps
      icons (Icon _, _, _) = True
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
            bmap <- readBitmapFile d w p
            case bmap of
                 Right (bw, bh, bp, _, _) -> do
                     addFinalizer bp (freePixmap d bp)
                     return $ Just $ Bitmap bw bh bp
                 Left err -> do
                     putStrLn err
                     return Nothing
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
    copyPlane d (pixmap i) p gc 0 0 w h x (1 + y - fromIntegral h `div` 2)  1
