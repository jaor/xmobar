{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XGraphic
-- Copyright   :  Copyright © 2013 Edward O'Callaghan. All Rights Reserved.
-- License     :  BSD3
--
-- Maintainer  :  Edward O'Callaghan - <victoredwardocallaghan@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module XGraphic
    ( readBitmapFile
    ) where

import Graphics.X11.Xlib
--import Graphics.X11.Xlib.Misc
import Foreign
import Foreign.C

-- | interface to the X11 library function @XWriteBitmapFile@.
readBitmapFile :: Display -> Drawable -> String
                  -> IO (Dimension, Dimension, Pixmap, Maybe CInt, Maybe CInt)
readBitmapFile display d filename =
  withCString filename $ \ c_filename ->
  alloca $ \ width_return ->
  alloca $ \ height_return ->
  alloca $ \ bitmap_return ->
  alloca $ \ x_hot_return ->
  alloca $ \ y_hot_return -> do
    _ <- xReadBitmapFile display d c_filename width_return height_return
         bitmap_return x_hot_return y_hot_return
    width <- peek width_return
    height <- peek height_return
    bitmap <- peek bitmap_return
    x_hot <- peek x_hot_return
    y_hot <- peek y_hot_return
    let m_x_hot | x_hot == -1 = Nothing
                | otherwise  = Just x_hot
        m_y_hot | y_hot == -1 = Nothing
                | otherwise  = Just y_hot
    return (fromIntegral width, fromIntegral height, bitmap, m_x_hot, m_y_hot)
foreign import ccall unsafe "X11/Xlib.h XReadBitmapFile"
  xReadBitmapFile :: Display -> Drawable -> CString -> Ptr CInt -> Ptr CInt
                     -> Ptr Pixmap -> Ptr CInt -> Ptr CInt -> IO CInt
