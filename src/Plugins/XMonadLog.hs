{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.StdinReader
-- Copyright   :  (c) Spencer Janssen
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <spencerjanssen@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin to display information from _XMONAD_LOG, specified at
-- http://code.haskell.org/XMonadContrib/XMonad/Hooks/DynamicLog.hs
--
-----------------------------------------------------------------------------

module Plugins.XMonadLog (XMonadLog(..)) where

import Control.Monad
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Plugins
#ifdef UTF8
#undef UTF8
import Codec.Binary.UTF8.String as UTF8
#define UTF8
#endif
import Foreign.C (CChar)
import XUtil (nextEvent')


data XMonadLog = XMonadLog | XPropertyLog String | NamedXPropertyLog String String
    deriving (Read, Show)

instance Exec XMonadLog where
    alias XMonadLog = "XMonadLog"
    alias (XPropertyLog atom) = atom
    alias (NamedXPropertyLog _ name) = name

    start x cb = do
        let atom = case x of
                XMonadLog             -> "_XMONAD_LOG"
                XPropertyLog      a   -> a
                NamedXPropertyLog a _ -> a
        d <- openDisplay ""
        xlog <- internAtom d atom False

        root  <- rootWindow d (defaultScreen d)
        selectInput d root propertyChangeMask

        let update = do
                        mwp <- getWindowProperty8 d xlog root
                        maybe (return ()) (cb . decodeCChar) mwp

        update

        allocaXEvent $ \ep -> forever $ do
            nextEvent' d ep
            e <- getEvent ep
            case e of
                PropertyEvent { ev_atom = a } | a ==  xlog -> update
                _ -> return ()

        return ()

decodeCChar :: [CChar] -> String
#ifdef UTF8
#undef UTF8
decodeCChar = UTF8.decode . map fromIntegral
#define UTF8
#else
decodeCChar = map (toEnum . fromIntegral)
#endif
