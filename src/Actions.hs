-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Actions
-- Copyright   :  (c) Alexander Polakov
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Actions (Action(..), runAction, stripActions) where

import System.Process (system)
import Control.Monad (void)
import Text.Regex (Regex, subRegex, mkRegex, matchRegex)
import Graphics.X11.Types (Button)

data Action = Spawn [Button] String
                deriving (Eq)

runAction :: Action -> IO ()
runAction (Spawn _ s) = void $ system (s ++ "&")

stripActions :: String -> String
stripActions s = case matchRegex actionRegex s of
  Nothing -> s
  Just _  -> stripActions strippedOneLevel
  where
      strippedOneLevel = subRegex actionRegex s $ "[action=\\1\\2]\\3[/action]"

actionRegex :: Regex
actionRegex = mkRegex "<action=`?([^>`]*)`?( +button=[12345]+)?>(.+)</action>"
