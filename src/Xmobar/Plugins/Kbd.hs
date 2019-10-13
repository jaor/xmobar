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

module Xmobar.Plugins.Kbd(Kbd(..)) where

import Data.List (isPrefixOf, findIndex)
import Data.Maybe (fromJust)
import Control.Monad (forever)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Xmobar.Run.Exec
import Xmobar.X11.Events (nextEvent')
import Xmobar.System.Kbd


-- 'Bad' prefixes of layouts
noLaySymbols :: [String]
noLaySymbols = ["group", "inet", "ctr", "pc", "ctrl", "terminate"]


-- splits the layout string into the actual layouts
splitLayout :: String -> [String]
splitLayout s = splitLayout' noLaySymbols $ split s '+'

splitLayout' :: [String] ->  [String] -> [String]
--                  end of recursion, remove empty strings
splitLayout' [] s = map (takeWhile (/= ':')) $ filter (not . null) s
--                    remove current string if it has a 'bad' prefix
splitLayout' bad s  =
  splitLayout' (tail bad) [x | x <- s, not $ isPrefixOf (head bad) x]

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
        x -> let i = fromJust x in snd $ opts!!i

-- returns the active layout
getKbdLay :: Display -> KbdOpts -> IO String
getKbdLay dpy opts = do
        lay <- getLayoutStr dpy
        curLay <- getKbdLayout dpy
        return $ searchReplaceLayout opts $ splitLayout lay!!curLay



newtype Kbd = Kbd [(String, String)]
  deriving (Read, Show)

instance Exec Kbd where
        alias (Kbd _) = "kbd"
        start (Kbd opts) cb = do

            dpy <- openDisplay ""

            -- initial set of layout
            cb =<< getKbdLay dpy opts

            -- enable listing for
            -- group changes
            _ <- xkbSelectEventDetails dpy xkbUseCoreKbd xkbStateNotify xkbAllStateComponentsMask xkbGroupStateMask
            -- layout/geometry changes
            _ <- xkbSelectEvents dpy  xkbUseCoreKbd xkbNewKeyboardNotifyMask xkbNewKeyboardNotifyMask

            allocaXEvent $ \e -> forever $ do
                nextEvent' dpy e
                _ <- getEvent e
                cb =<< getKbdLay dpy opts

            closeDisplay dpy
            return ()
