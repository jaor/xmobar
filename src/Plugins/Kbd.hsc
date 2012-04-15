{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface, MultiParamTypeClasses, DeriveDataTypeable, FlexibleInstances, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Kbd
-- Copyright   :  (c) Martin Perner
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Martin Perner <martin@perner.cc>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A keyboard layout indicator for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Kbd where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Plugins
import Control.Monad (forever)
import XUtil (nextEvent')
import Data.List (isPrefixOf, findIndex)
import Data.Maybe (fromJust)

#include <X11/XKBlib.h>
#include <X11/extensions/XKB.h>
#include <X11/extensions/XKBstr.h>

-- 
-- Definition for XkbStaceRec and getKbdLayout taken from
-- XMonad.Layout.XKBLayout
--
data XkbStateRec = XkbStateRec {
    group :: CUChar,
    locked_group :: CUChar,
    base_group :: CUShort,
    latched_group :: CUShort,
    mods :: CUChar,
    base_mods :: CUChar,
    latched_mods :: CUChar,
    locked_mods :: CUChar,
    compat_state :: CUChar,
    grab_mods :: CUChar,
    compat_grab_mods :: CUChar,
    lookup_mods :: CUChar,
    compat_lookup_mods :: CUChar,
    ptr_buttons :: CUShort
}

instance Storable XkbStateRec where
    sizeOf _ = (#size XkbStateRec)
    alignment _ = alignment (undefined :: CUShort)
    peek ptr = do
        r_group <- (#peek XkbStateRec, group) ptr
        r_locked_group <- (#peek XkbStateRec, locked_group) ptr
        r_base_group <- (#peek XkbStateRec, base_group) ptr
        r_latched_group <- (#peek XkbStateRec, latched_group) ptr
        r_mods <- (#peek XkbStateRec, mods) ptr
        r_base_mods <- (#peek XkbStateRec, base_mods) ptr
        r_latched_mods <- (#peek XkbStateRec, latched_mods) ptr
        r_locked_mods <- (#peek XkbStateRec, locked_mods) ptr
        r_compat_state <- (#peek XkbStateRec, compat_state) ptr
        r_grab_mods <- (#peek XkbStateRec, grab_mods) ptr
        r_compat_grab_mods <- (#peek XkbStateRec, compat_grab_mods) ptr
        r_lookup_mods <- (#peek XkbStateRec, lookup_mods) ptr
        r_compat_lookup_mods <- (#peek XkbStateRec, compat_lookup_mods) ptr
        r_ptr_buttons <- (#peek XkbStateRec, ptr_buttons) ptr
        return XkbStateRec {
            group = r_group,
            locked_group = r_locked_group,
            base_group = r_base_group,
            latched_group = r_latched_group,
            mods = r_mods,
            base_mods = r_base_mods,
            latched_mods = r_latched_mods,
            locked_mods = r_locked_mods,
            compat_state = r_compat_state,
            grab_mods = r_grab_mods,
            compat_grab_mods = r_compat_grab_mods,
            lookup_mods = r_lookup_mods,
            compat_lookup_mods = r_compat_lookup_mods,
            ptr_buttons = r_ptr_buttons
        }

foreign import ccall unsafe "X11/XKBlib.h XkbGetState"
    xkbGetState :: Display -> CUInt -> Ptr XkbStateRec -> IO CInt


getKbdLayout :: Display -> IO Int
getKbdLayout d = alloca $ \stRecPtr -> do
    xkbGetState d 0x100 stRecPtr
    st <- peek stRecPtr
    return $ fromIntegral (group st)

--
--
--

data XkbKeyNameRec = XkbKeyNameRec {
	name :: Ptr CChar -- array
}

--
-- the t_ before alias is just because of name collisions
--
data XkbKeyAliasRec = XkbKeyAliasRec {
	real  :: Ptr CChar, -- array
	t_alias :: Ptr CChar  -- array
} 

--
-- the t_ before geometry is just because of name collisions
--
data XkbNamesRec = XkbNamesRec {
    keycodes :: Atom,
    t_geometry :: Atom,
    symbols :: Atom,
    types :: Atom,
    compat :: Atom,
    vmods :: Ptr Atom,
    indicators :: Ptr Atom, -- array
    groups :: Ptr Atom, -- array
    keys :: Ptr XkbKeyNameRec,
    key_aliases :: Ptr CChar, -- dont care XkbKeyAliasRec,
    radio_groups :: Ptr Atom,
    phys_symbols :: Atom,
    num_keys :: CUChar,
    num_key_aliases :: CUChar,
    num_rg :: CUShort
}

--
-- the t_ before map, indicators and compat are just because of name collisions
--
data XkbDescRec = XkbDescRec {
    t_dpy :: Ptr CChar, -- struct _XDisplay* ; don't care
    flags :: CUShort,
    device_spec :: CUShort,
    min_key_code :: KeyCode,
    max_key_code :: KeyCode,
    ctrls :: Ptr CChar, -- XkbControlsPtr ;  dont' care
    server :: Ptr CChar, -- XkbServerMapPtr ;  dont' care
    t_map :: Ptr CChar, --XkbClientMapPtr ;  dont' care
    t_indicators :: Ptr CChar, -- XkbIndicatorPtr ;  dont' care
	names :: Ptr XkbNamesRec, -- array
    t_compat :: Ptr CChar, -- XkbCompatMap ;  dont' care
    geom :: Ptr CChar -- XkbGeometryPtr ;  dont' care

}

instance Storable XkbKeyNameRec where
    sizeOf _ = (#size XkbKeyNameRec)
    alignment _ = alignment (undefined :: CUShort)
    peek ptr = do
        r_name <- (#peek XkbKeyNameRec, name) ptr

        return XkbKeyNameRec {
            name = r_name
        }

instance Storable XkbKeyAliasRec where
    sizeOf _ = (#size XkbKeyAliasRec)
    alignment _ = alignment (undefined :: CUShort)
    peek ptr = do
        r_real <- (#peek XkbKeyAliasRec, real) ptr
        r_alias <- (#peek XkbKeyAliasRec, alias) ptr

        return XkbKeyAliasRec {
            real = r_real,
            t_alias = r_alias
        }

instance Storable XkbNamesRec where
    sizeOf _ = (#size XkbNamesRec)
    alignment _ = alignment (undefined :: CUShort)
    peek ptr = do
        r_keycodes <- (#peek XkbNamesRec, keycodes) ptr
        r_geometry <- (#peek XkbNamesRec, geometry) ptr
        r_symbols <- (#peek XkbNamesRec, symbols ) ptr
        r_types <- (#peek XkbNamesRec, types ) ptr
        r_compat <- (#peek XkbNamesRec, compat ) ptr  
        r_vmods <- (#peek XkbNamesRec,  vmods ) ptr
        r_indicators <- (#peek XkbNamesRec, indicators ) ptr  
        r_groups <- (#peek XkbNamesRec, groups ) ptr 
        r_keys <- (#peek XkbNamesRec, keys ) ptr   
        r_key_aliases <- (#peek XkbNamesRec, key_aliases  ) ptr
        r_radio_groups <- (#peek XkbNamesRec, radio_groups  ) ptr 
        r_phys_symbols <- (#peek XkbNamesRec, phys_symbols ) ptr  
        r_num_keys <- (#peek XkbNamesRec,num_keys  ) ptr
        r_num_key_aliases <- (#peek XkbNamesRec, num_key_aliases  ) ptr
        r_num_rg <- (#peek XkbNamesRec, num_rg ) ptr

        return XkbNamesRec {
            keycodes = r_keycodes,
            t_geometry = r_geometry,
            symbols = r_symbols,
            types = r_types,
            compat = r_compat,
            vmods = r_vmods,
            indicators = r_indicators,
            groups = r_groups,
            keys = r_keys,
            key_aliases = r_key_aliases,
            radio_groups = r_radio_groups,
            phys_symbols = r_phys_symbols,
            num_keys = r_num_keys,
            num_key_aliases = r_num_key_aliases,
            num_rg = r_num_rg
       }

instance Storable XkbDescRec where
    sizeOf _ = (#size XkbDescRec)
    alignment _ = alignment (undefined :: CUShort)
    peek ptr = do
        r_dpy <- (#peek XkbDescRec, dpy) ptr
        r_flags <- (#peek XkbDescRec, flags) ptr
        r_device_spec <- (#peek XkbDescRec, device_spec) ptr
        r_min_key_code <- (#peek XkbDescRec, min_key_code) ptr
        r_max_key_code <- (#peek XkbDescRec, max_key_code) ptr
        r_ctrls <- (#peek XkbDescRec, ctrls) ptr
        r_server <- (#peek XkbDescRec, server) ptr
        r_map <- (#peek XkbDescRec, map) ptr
        r_indicators <- (#peek XkbDescRec, indicators) ptr
        r_names <- (#peek XkbDescRec, names) ptr
        r_compat <- (#peek XkbDescRec, compat) ptr
        r_geom <- (#peek XkbDescRec, geom) ptr

        return XkbDescRec {
            t_dpy = r_dpy,
            flags = r_flags,
            device_spec = r_device_spec,
            min_key_code = r_min_key_code,
            max_key_code = r_max_key_code,
            ctrls = r_ctrls,
            server = r_server,
            t_map = r_map,
            t_indicators = r_indicators,
            names = r_names,
            t_compat = r_compat,
            geom = r_geom
        }

--
-- C bindings
--

foreign import ccall unsafe "X11/XKBlib.h XkbAllocKeyboard"
    xkbAllocKeyboard :: IO (Ptr XkbDescRec)

foreign import ccall unsafe "X11/XKBlib.h XkbGetNames"
    xkbGetNames :: Display -> CUInt -> (Ptr XkbDescRec)  -> IO Status

foreign import ccall unsafe "X11/XKBlib.h XGetAtomName"
    xGetAtomName :: Display -> Atom -> IO CString

foreign import ccall unsafe "X11/XKBlib.h XkbFreeNames"
    xkbFreeNames :: (Ptr XkbDescRec) -> CUInt -> CInt -> IO ()

foreign import ccall unsafe "X11/XKBlib.h XkbFreeKeyboard"
    xkbFreeKeyboard :: (Ptr XkbDescRec) -> CUInt -> CInt -> IO ()

foreign import ccall unsafe "X11/XKBlib.h XkbSelectEventDetails"
	xkbSelectEventDetails :: Display -> CUInt -> CUInt -> CULong -> CULong -> IO CUInt

foreign import ccall unsafe "X11/XKBlib.h XkbSelectEvents"
	xkbSelectEvents :: Display -> CUInt -> CUInt -> CUInt -> IO CUInt


xkbUseCoreKbd :: CUInt
xkbUseCoreKbd = #const XkbUseCoreKbd

xkbStateNotify :: CUInt
xkbStateNotify = #const XkbStateNotify

xkbMapNotify :: CUInt
xkbMapNotify = #const XkbMapNotify

xkbMapNotifyMask :: CUInt
xkbMapNotifyMask = #const XkbMapNotifyMask

xkbNewKeyboardNotifyMask :: CUInt
xkbNewKeyboardNotifyMask  = #const XkbNewKeyboardNotifyMask

xkbAllStateComponentsMask :: CULong
xkbAllStateComponentsMask = #const XkbAllStateComponentsMask

xkbGroupStateMask :: CULong
xkbGroupStateMask = #const XkbGroupStateMask

xkbSymbolsNameMask :: CUInt
xkbSymbolsNameMask = #const XkbSymbolsNameMask

xkbGroupNamesMask :: CUInt
xkbGroupNamesMask = #const XkbGroupNamesMask

type KbdOpts = [(String, String)]

-- gets the layout string
getLayoutStr :: Display -> IO String
getLayoutStr dpy =  do
        kbdDescPtr <- xkbAllocKeyboard
        status <- xkbGetNames dpy xkbSymbolsNameMask kbdDescPtr
        str <- getLayoutStr' status dpy kbdDescPtr
        xkbFreeNames kbdDescPtr xkbGroupNamesMask 1
        xkbFreeKeyboard kbdDescPtr 0 1
        return str

getLayoutStr' :: Status -> Display -> (Ptr XkbDescRec) -> IO String
getLayoutStr' st dpy kbdDescPtr =
        if st == 0 then -- Success
            do
            kbdDesc <- peek kbdDescPtr
            nameArray <- peek (names kbdDesc)
            atom <- xGetAtomName dpy (symbols nameArray)
            str <- peekCString atom
            return str
        else -- Behaviour on error
            do
                return "Error while requesting layout!"


-- 'Bad' prefixes of layouts
noLaySymbols :: [String]
noLaySymbols = ["group", "inet", "ctr", "pc", "ctrl"]


-- splits the layout string into the actual layouts
splitLayout :: String -> [String]
splitLayout s = splitLayout' noLaySymbols $ split s '+' 

splitLayout' :: [String] ->  [String] -> [String]
--                  end of recursion, remove empty strings
splitLayout' [] s = map (takeWhile (\x -> x /= ':')) $ filter (\x -> length x > 0) s
--                    remove current string if it has a 'bad' prefix
splitLayout' bad s  = splitLayout' (tail bad) [x | x <- s, not $ isPrefixOf (head bad) x]

-- split String at each Char
split :: String -> Char -> [String]
split [] _ = [""]
split (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
        where
            rest = split cs delim

-- replaces input string if on search list (exact match) with corresponding
-- element on replacement list.
-- 
-- if not found, return string unchanged
searchReplaceLayout :: KbdOpts -> String -> String
searchReplaceLayout opts s = let c = findIndex (\x -> fst x == s) opts in
    case c of
        Nothing -> s
        x -> let i = (fromJust x) in
            snd $ opts!!i

-- returns the active layout
getKbdLay :: Display -> KbdOpts -> IO String
getKbdLay dpy opts = do
        lay <- getLayoutStr dpy
        curLay <- getKbdLayout dpy
        return $ searchReplaceLayout opts $ (splitLayout lay)!!(curLay)



data Kbd = Kbd [(String, String)]
	deriving (Read, Show)

instance Exec Kbd where
	alias (Kbd _) = "kbd"
	start (Kbd opts) cb = do

        dpy <- openDisplay ""

        -- initial set of layout
        cb =<< (getKbdLay dpy opts)

        -- enable listing for
        -- group changes
        _ <- xkbSelectEventDetails dpy xkbUseCoreKbd xkbStateNotify xkbAllStateComponentsMask xkbGroupStateMask
        -- layout/geometry changes
        _ <- xkbSelectEvents dpy  xkbUseCoreKbd xkbNewKeyboardNotifyMask xkbNewKeyboardNotifyMask

        allocaXEvent $ \e -> forever $ do
            nextEvent' dpy e
            _ <- getEvent e
            cb =<< (getKbdLay dpy opts)

        closeDisplay dpy
        return ()

-- vim:ft=haskell:ts=4:shiftwidth=4:softtabstop=4:expandtab:foldlevel=20:

