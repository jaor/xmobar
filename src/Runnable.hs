{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Runnable
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The existential type to store the list of commands to be executed.
-- I must thank Claus Reinke for the help in understanding the mysteries of
-- reading existential types. The Read instance of Runnable must be credited to
-- him.
--
-- See here:
-- http:\/\/www.haskell.org\/pipermail\/haskell-cafe\/2007-July\/028227.html
--
-----------------------------------------------------------------------------

module Runnable where

import Control.Monad
import Text.Read
import Config (runnableTypes)
import Commands

data Runnable = forall r . (Exec r, Read r, Show r) => Run r

instance Exec Runnable where
     start   (Run a) = start   a
     alias   (Run a) = alias   a
     trigger (Run a) = trigger a

instance Show Runnable where
    show (Run x) = show x

instance Read Runnable where
    readPrec = readRunnable

class ReadAsAnyOf ts ex where
    -- | Reads an existential type as any of hidden types ts
    readAsAnyOf :: ts -> ReadPrec ex

instance ReadAsAnyOf () ex where
    readAsAnyOf ~() = mzero

instance (Show t, Read t, Exec t, ReadAsAnyOf ts Runnable) => ReadAsAnyOf (t,ts) Runnable where
    readAsAnyOf ~(t,ts) = r t `mplus` readAsAnyOf ts
              where r ty = do { m <- readPrec; return (Run (m `asTypeOf` ty)) }

-- | The 'Prelude.Read' parser for the 'Runnable' existential type. It
-- needs an 'Prelude.undefined' with a type signature containing the
-- list of all possible types hidden within 'Runnable'. See 'Config.runnableTypes'.
-- Each hidden type must have a 'Prelude.Read' instance.
readRunnable :: ReadPrec Runnable
readRunnable = prec 10 $ do
                 Ident "Run" <- lexP
                 parens $ readAsAnyOf runnableTypes
