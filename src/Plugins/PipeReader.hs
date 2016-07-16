-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.PipeReader
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for reading from named pipes
--
-----------------------------------------------------------------------------

module Plugins.PipeReader where

import System.IO
import Plugins
import Environment
import System.Posix.Files
import Control.Concurrent(threadDelay)
import Control.Exception
import Control.Monad(forever, unless)

data PipeReader = PipeReader String String
    deriving (Read, Show)

instance Exec PipeReader where
    alias (PipeReader _ a)    = a
    start (PipeReader p _) cb = do
        (def, pipe) <- split ':' <$> expandEnv p
        unless (null def) (cb def)
        checkPipe pipe
        h <- openFile pipe ReadWriteMode
        forever (hGetLineSafe h >>= cb)
      where
        split c xs | c `elem` xs = let (pre, post) = span (c /=) xs
                                   in (pre, dropWhile (c ==) post)
                   | otherwise   = ([], xs)

checkPipe :: FilePath -> IO ()
checkPipe file =
    handle (\(SomeException _) -> waitForPipe) $ do
        status <- getFileStatus file
        unless (isNamedPipe status) waitForPipe
    where waitForPipe = threadDelay 1000000 >> checkPipe file
