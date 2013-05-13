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
import System.Posix.Files
import Control.Concurrent(threadDelay)
import Control.Exception
import Control.Monad(when)

data PipeReader = PipeReader String String
    deriving (Read, Show)

instance Exec PipeReader where
    alias (PipeReader _ a)    = a
    start (PipeReader p _) cb = do
        let (def, pipe) = split ':' p
        when (not $ null def) (cb def)
        checkPipe pipe
        h <- openFile pipe ReadWriteMode
        forever (hGetLineSafe h >>= cb)
      where
        forever a = a >> forever a
        split c xs | c `elem` xs = let (pre, post) = span ((/=) c) xs
                                   in (pre, dropWhile ((==) c) post)
                   | otherwise   = ([], xs)

checkPipe :: FilePath -> IO ()
checkPipe file = do
    handle (\(SomeException _) -> waitForPipe) $ do
    status <- getFileStatus file
    if isNamedPipe status
      then return ()
      else waitForPipe
    where waitForPipe = threadDelay 1000 >> checkPipe file
