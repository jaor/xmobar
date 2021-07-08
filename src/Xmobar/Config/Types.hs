-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Config
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The configuration types
--
-----------------------------------------------------------------------------

module Xmobar.Config.Types
    ( -- * Configuration
      -- $config
      Config (..)
    , XPosition (..), Align (..), Border(..)
    ) where

import Xmobar.Run.Runnable (Runnable(..))

-- $config
-- Configuration data type

-- | The configuration data type
data Config =
    Config { font :: String         -- ^ Font
           , additionalFonts :: [String] -- ^ List of alternative fonts
           , wmClass :: String      -- ^ X11 WM_CLASS property value
           , wmName :: String       -- ^ X11 WM_NAME property value
           , bgColor :: String      -- ^ Backgroud color
           , fgColor :: String      -- ^ Default font color
           , position :: XPosition  -- ^ Top Bottom or Static
           , textOffset :: Int      -- ^ Offset from top of window for text
           , textOffsets :: [Int]   -- ^ List of offsets for additionalFonts
           , iconOffset :: Int      -- ^ Offset from top of window for icons
           , border :: Border       -- ^ NoBorder TopB BottomB or FullB
           , borderColor :: String  -- ^ Border color
           , borderWidth :: Int     -- ^ Border width
           , alpha :: Int           -- ^ Transparency from 0 (transparent)
                                    --   to 255 (opaque)
           , hideOnStart :: Bool    -- ^ Hide (Unmap) the window on
                                    --   initialization
           , allDesktops :: Bool    -- ^ Tell the WM to map to all desktops
           , overrideRedirect :: Bool -- ^ Needed for dock behaviour in some
                                      --   non-tiling WMs
           , pickBroadest :: Bool   -- ^ Use the broadest display
                                    --   instead of the first one by
                                    --   default
           , lowerOnStart :: Bool   -- ^ lower to the bottom of the
                                    --   window stack on initialization
           , persistent :: Bool     -- ^ Whether automatic hiding should
                                    --   be enabled or disabled
           , iconRoot :: FilePath   -- ^ Root folder for icons
           , commands :: [Runnable] -- ^ For setting the command,
                                    --   the command arguments
                                    --   and refresh rate for the programs
                                    --   to run (optional)
           , sepChar :: String      -- ^ The character to be used for indicating
                                    --   commands in the output template
                                    --   (default '%')
           , alignSep :: String     -- ^ Separators for left, center and
                                    --   right text alignment
           , template :: String     -- ^ The output template
           , verbose :: Bool        -- ^ Emit additional debug messages
           } deriving (Read, Show)

data XPosition = Top
               | TopH Int
               | TopW Align Int
               | TopSize Align Int Int
               | TopP Int Int
               | Bottom
               | BottomH Int
               | BottomP Int Int
               | BottomW Align Int
               | BottomSize Align Int Int
               | Static {xpos, ypos, width, height :: Int}
               | OnScreen Int XPosition
                 deriving ( Read, Show, Eq )

data Align = L | R | C deriving ( Read, Show, Eq )

data Border = NoBorder
            | TopB
            | BottomB
            | FullB
            | TopBM Int
            | BottomBM Int
            | FullBM Int
              deriving ( Read, Show, Eq )
