{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.Compile
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Mon Nov 26, 2018 03:36
--
--
-- Utlities to compile xmobar executables on the fly
--
------------------------------------------------------------------------------


module Xmobar.App.Compile(recompile, trace, xmessage) where

import Control.Monad.IO.Class
import Control.Monad.Fix (fix)
import Control.Exception.Extensible (try, bracket, SomeException(..))
import qualified Control.Exception.Extensible as E
import Control.Monad (filterM, when)
import Data.List ((\\))
import Data.Maybe (isJust)
import System.FilePath((</>), takeExtension)
import System.IO
import System.Directory
import System.Process
import System.Exit
import System.Posix.Process(executeFile, forkProcess, getAnyProcessStatus)
import System.Posix.Types(ProcessID)
import System.Posix.Signals

isExecutable :: FilePath -> IO Bool
isExecutable f =
  E.catch (executable <$> getPermissions f) (\(SomeException _) -> return False)

checkBuildScript :: Bool -> FilePath -> IO Bool
checkBuildScript verb buildscript = do
  exists <- doesFileExist buildscript
  if exists
    then do
      isExe <- isExecutable buildscript
      if isExe
        then do
          trace verb $ "Xmobar will use build script at "
                       ++ show buildscript ++ " to recompile."
          return True
        else do
          trace verb $ unlines
            [ "Xmobar will not use build script, because "
              ++ show buildscript ++ " is not executable."
            , "Suggested resolution to use it: chmod u+x "
              ++ show buildscript
            ]
          return False
    else do
      trace verb $ "Xmobar will use ghc to recompile, because "
                   ++ show buildscript ++ " does not exist."
      return False

shouldRecompile :: Bool -> FilePath -> FilePath -> FilePath -> IO Bool
shouldRecompile verb src bin lib = do
  libTs <- mapM getModTime . filter isSource =<< allFiles lib
  srcT <- getModTime src
  binT <- getModTime bin
  if any (binT <) (srcT : libTs)
    then do
      trace verb "Xmobar recompiling because some files have changed."
      return True
    else do
      trace verb $ "Xmobar skipping recompile because it is not forced "
                   ++ "(e.g. via --recompile), and not any *.hs / *.lhs / *.hsc"
                   ++ " files in lib/ have been changed."
      return False
  where isSource = flip elem [".hs",".lhs",".hsc"] . takeExtension
        allFiles t = do
            let prep = map (t</>) . Prelude.filter (`notElem` [".",".."])
            cs <- prep <$> E.catch (getDirectoryContents t)
                                   (\(SomeException _) -> return [])
            ds <- filterM doesDirectoryExist cs
            concat . ((cs \\ ds):) <$> mapM allFiles ds
        getModTime f = E.catch (Just <$> getModificationTime f)
                               (\(SomeException _) -> return Nothing)

runProc :: FilePath -> [String] -> FilePath -> Handle -> IO ProcessHandle
runProc bin args dir eh =
  runProcess bin args (Just dir) Nothing Nothing Nothing (Just eh)

xmessage :: String -> IO System.Posix.Types.ProcessID
xmessage msg = forkProcess $
  executeFile "xmessage" True ["-default", "okay", replaceUnicode msg] Nothing
  where -- Replace some of the unicode symbols GHC uses in its output
        replaceUnicode = map $ \c -> case c of
         '\8226' -> '*'  -- •
         '\8216' -> '`'  -- ‘
         '\8217' -> '`'  -- ’
         _ -> c

ghcErrorMsg :: (Monad m, Show a) => String -> a -> String -> m String
ghcErrorMsg src status ghcErr = return . unlines $
  ["Error detected while loading xmobar configuration file: " ++ src]
  ++ lines (if null ghcErr then show status else ghcErr)
  ++ ["","Please check the file for errors."]

-- | A 'trace' for the 'X' monad. Logs a string to stderr. The result may
-- be found in your .xsession-errors file
trace :: MonadIO m => Bool -> String -> m ()
trace verb msg = when verb (liftIO $ hPutStrLn stderr msg)

-- | 'recompile force', recompile the xmobar configuration file when
-- any of the following apply:
--
--      * force is 'True'
--
--      * the execName executable does not exist
--
--      * the xmobar executable is older than .hs or any file in
--        the @lib@ directory (under the configuration directory).
--
-- The -i flag is used to restrict recompilation to the xmobar.hs file only,
-- and any files in the aforementioned @lib@ directory.
--
-- Compilation errors (if any) are logged to the @xmobar.errors@ file
-- in the given directory.  If GHC indicates failure with a
-- non-zero exit code, an xmessage displaying that file is spawned.
--
-- 'False' is returned if there are compilation errors.
--
recompile :: MonadIO m => String -> String -> String -> Bool -> Bool -> m Bool
recompile confDir dataDir execName force verb = liftIO $ do
    let bin  = confDir </> execName
        err  = dataDir </> (execName ++ ".errors")
        src  = confDir </> (execName ++ ".hs")
        lib  = confDir </> "lib"
        script = confDir </> "build"
    useScript <- checkBuildScript verb script
    sc <- if useScript || force
          then return True
          else shouldRecompile verb src bin lib
    if sc
      then do
        uninstallSignalHandlers
        status <- bracket (openFile err WriteMode) hClose $
                    \errHandle ->
                      waitForProcess =<<
                        if useScript
                        then runScript script bin confDir errHandle
                        else runGHC bin confDir errHandle
        installSignalHandlers
        if status == ExitSuccess
            then trace verb "Xmobar recompilation process exited with success!"
            else do
                msg <- readFile err >>= ghcErrorMsg src status
                hPutStrLn stderr msg
                exitWith (ExitFailure 1)
        return (status == ExitSuccess)
      else return True
 where opts bin = ["--make" , execName ++ ".hs" , "-i" , "-ilib"
                  , "-fforce-recomp" , "-main-is", "main" , "-v0"]
#ifdef THREADED_RUNTIME
                  ++ ["-threaded"]
#endif
#ifdef DRTSOPTS
                  ++ ["-rtsopts", "-with-rtsopts", "-V0"]
#endif
                  ++ ["-o", bin]
       runGHC bin = runProc "ghc" (opts bin)
       runScript script bin = runProc script [bin]

-- | Ignore SIGPIPE to avoid termination when a pipe is full, and SIGCHLD to
-- avoid zombie processes, and clean up any extant zombie processes.
installSignalHandlers :: MonadIO m => m ()
installSignalHandlers = liftIO $ do
    installHandler openEndedPipe Ignore Nothing
    installHandler sigCHLD Ignore Nothing
    (try :: IO a -> IO (Either SomeException a))
      $ fix $ \more -> do
        x <- getAnyProcessStatus False False
        when (isJust x) more
    return ()

uninstallSignalHandlers :: MonadIO m => m ()
uninstallSignalHandlers = liftIO $ do
    installHandler openEndedPipe Default Nothing
    installHandler sigCHLD Default Nothing
    return ()
