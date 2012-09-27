-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Locks
-- Copyright   :  (c) Patrick Chilton
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Patrick Chilton <chpatrick@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin that displays the status of the lock keys.
--
-----------------------------------------------------------------------------

module Plugins.Locks where

import Graphics.X11
import Data.List
import Data.Bits
import Control.Monad
import Graphics.X11.Xlib.Extras
import Plugins

data Locks = Locks
    deriving (Read, Show)

locks :: [ ( KeySym, String )]
locks = [ ( xK_Caps_Lock,   "CAPS"   )
        , ( xK_Num_Lock,    "NUM"    ) 
        , ( xK_Scroll_Lock, "SCROLL" ) 
        ]

instance Exec Locks where
    alias Locks = "locks"
    rate Locks = 2
    run Locks = do
        d <- openDisplay ""
        root <- rootWindow d (defaultScreen d)

        modMap <- getModifierMapping d
        ( _, _, _, _, _, _, _, m ) <- queryPointer d root

        ls <- filterM ( \( ks, _ ) -> do
            kc <- keysymToKeycode d ks
            return $ case find (elem kc . snd) modMap of
                Nothing       -> False
                Just ( i, _ ) -> testBit m (fromIntegral i)
            ) locks
        closeDisplay d

        return $ unwords $ map snd ls 