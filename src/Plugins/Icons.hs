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
import Control.Concurrent (threadDelay)

data Icons = Icons
    deriving (Read, Show)


instance Exec Icons where
    alias Icons = "icons"
    rate Icons = 2
    run Icons = do
    d <- openDisplay ""
    root <- rootWindow d (defaultScreen d)
    drawInWin d root "cat.xbm" -- FIXME: hardcoded icon
    sync d False
    threadDelay (1 * 1000000)
    closeDisplay d
    return "<fc=red>Hello World!!</fc>"


drawInWin :: Display -> Window -> String -> IO ()
drawInWin dpy win str = do
 gc <- createGC dpy win
 (bitmap_width, bitmap_height, p, _, _) <- readBitmapFile dpy win str
 copyPlane dpy p win gc 0 0 bitmap_width bitmap_height 0 0 1
 freeGC dpy gc
 freePixmap dpy p
