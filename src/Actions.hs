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

module Actions where

import System.Process (system)
import Control.Monad (void)

data Action = Spawn String
                deriving (Eq)

runAction :: Action -> IO ()
runAction (Spawn s) = void $ system (s ++ "&")
