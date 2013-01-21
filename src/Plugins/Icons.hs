-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Icons
-- Copyright   :  Copyright © 2013 Edward O'Callaghan. All Rights Reserved.
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Edward O'Callaghan <victoredwardocallaghan@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin that displays icons for each status parameter.
--
-----------------------------------------------------------------------------

module Plugins.Icons where

import Plugins
import XGraphic
import Graphics.X11
import Graphics.X11.Xlib
import Graphics.X11.Types
import Foreign.C.Types
import Graphics.X11.Xlib.Misc
import Graphics.X11.Xlib.Context
import Graphics.X11.Xlib.Extras
import Data.List
import Data.Bits
import Control.Monad
import System.FilePath
import XUtil
import Control.Concurrent (threadDelay)

data Icons = Icons
    deriving (Read, Show)

--readXBM :: Display -> Drawable -> String -> IO Pixmap
--readXBM display window filename = do
   -- graphical context
--   GC gc

   -- this variable will contain the ID of the newly created pixmap.
--   Pixmap bitmap

   -- these variables will contain the dimensions of the loaded bitmap.
--   Dimension bitmap_width bitmap_height

   -- these variables will contain the location of the hotspot of the loaded bitmap.
--   CInt hotspot_x hotspot_y

   -- load the bitmap file in FilePath then create pixmap
   -- containing its data in the server, and put its ID in the 'bitmap' variable.
   -- FIXME: failure handle..
--readBitmapFile :: Display -> Drawable -> String
--                  -> IO (Dimension, Dimension, Pixmap, Maybe CInt, Maybe CInt)
--   readBitmapFile( display window
--                   filename
--                   bitmap_width bitmap_height
--                   bitmap
--                   hotspot_x hotspot_y
--                 )

--   (bitmap_width, bitmap_height, bitmap, _, _) <- 
--                      readBitmapFile display window filename

-- createGC :: Display -> Drawable -> IO GC
--   gc <- io $ createGC display window

-- copyPlane :: Display -> Drawable -> Drawable -> GC -> Position -> Position -> Dimension -> Dimension -> Position -> Position -> Pixel -> IO ())
--   copyPlane display bitmap window gc
--              0 0
--              bitmap_width bitmap_height
--              bitmap_width bitmap_height
--              1
--   return bitmap
--    /* check for failure or success. */
--    switch (rc) {
--        case BitmapOpenFailed:
--            fprintf(stderr, "XReadBitmapFile - could not open file 'icon.bmp'.\n");
--	    exit(1);
--            break;
--        case BitmapFileInvalid:
--            fprintf(stderr,
--                    "XReadBitmapFile - file '%s' doesn't contain a valid bitmap.\n",
--                    "icon.bmp");
--	    exit(1);
--            break;
--        case BitmapNoMemory:
--            fprintf(stderr, "XReadBitmapFile - not enough memory.\n");
--	    exit(1);
--            break;
--    }
--

drawInWin :: Display -> Window -> String -> IO ()
drawInWin dpy win str = do
 gc <- createGC dpy win
 --p <- createPixmap dpy win 200 100 (defaultDepthOfScreen (defaultScreenOfDisplay dpy))
 (bitmap_width, bitmap_height, p, _, _) <- readBitmapFile dpy win str
 copyArea dpy p win gc 0 0 200 100 0 0
 freeGC dpy gc
 freePixmap dpy p


instance Exec Icons where
    alias Icons = "icons"
    rate Icons = 2
    run Icons = do
    d <- openDisplay ""
    root <- rootWindow d (defaultScreen d)
    --bitmap <- readXBM d root "~/Current_Work/Haskell/xmobar/cat.xbm"
    drawInWin d root "cat.xbm"
    sync d False
 --       modMap <- getModifierMapping d
 --       ( _, _, _, _, _, _, _, m ) <- queryPointer d root
 --       ls <- filterM ( \( ks, _ ) -> do
 --           kc <- keysymToKeycode d ks
 --           return $ case find (elem kc . snd) modMap of
 --               Nothing       -> False
 --               Just ( i, _ ) -> testBit m (fromIntegral i)
 --           ) locks
    closeDisplay d
    return "<fc=red>Hello World!!</fc>"
--        return $ unwords $ map snd ls
