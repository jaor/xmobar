-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Alsa
-- Copyright   :  (c) 2018 Daniel Schüssler
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Event-based variant of the Volume plugin.
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}
module Xmobar.Plugins.Monitors.Alsa
  ( startAlsaPlugin
  , withMonitorWaiter
  , parseOptsIncludingMonitorArgs
  , AlsaOpts(aoAlsaCtlPath)
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Time.Clock
import Xmobar.Plugins.Monitors.Common
import qualified Xmobar.Plugins.Monitors.Volume as Volume;
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.IO
import System.IO.Error
import System.Process

alsaCtlRestartRateLimit :: NominalDiffTime
alsaCtlRestartRateLimit = 3 -- 'Num NominalDiffTime' assumes seconds

data AlsaOpts = AlsaOpts
    { aoVolumeOpts :: Volume.VolumeOpts
    , aoAlsaCtlPath :: Maybe FilePath
    }

defaultOpts :: AlsaOpts
defaultOpts = AlsaOpts Volume.defaultOpts Nothing

alsaCtlOptionName :: String
alsaCtlOptionName = "alsactl"

options :: [OptDescr (AlsaOpts -> AlsaOpts)]
options =
    Option "" [alsaCtlOptionName] (ReqArg (\x o ->
       o { aoAlsaCtlPath = Just x }) "") ""
    : fmap (fmap modifyVolumeOpts) Volume.options
  where
    modifyVolumeOpts f o = o { aoVolumeOpts = f (aoVolumeOpts o) }

-- | Drop generic Monitor args first, then apply 'parseOptsWith' in order to
-- parse everything.
parseOptsIncludingMonitorArgs :: [String] -> IO AlsaOpts
parseOptsIncludingMonitorArgs args =
    case getOpt Permute [] args of
        (_, args', _) -> parseOptsWith options defaultOpts args'

startAlsaPlugin :: String -> String -> [String] -> (String -> IO ()) -> IO ()
startAlsaPlugin mixerName controlName args cb = do
  opts <- parseOptsIncludingMonitorArgs args

  let run args2 = do
        -- Replicating the reparsing logic used by other plugins for now,
        -- but it seems the option parsing could be floated out (actually,
        -- GHC could in principle do it already since getOpt is pure, but
        -- it would have to inline 'runMBD', 'doArgs' and 'parseOpts' to see
        -- it, which probably isn't going to happen with the default
        -- optimization settings).
        opts2 <- io $ parseOptsWith options defaultOpts args2
        Volume.runVolumeWith (aoVolumeOpts opts2) mixerName controlName

  withMonitorWaiter mixerName (aoAlsaCtlPath opts) cb $ \wait_ ->
    runMB args Volume.volumeConfig run wait_ cb

withMonitorWaiter :: String -> Maybe FilePath -> (String -> IO ()) -> (IO () -> IO a) -> IO a
withMonitorWaiter mixerName alsaCtlPathOverride outputCallback cont = do
  mvar <- newMVar ()

  effectivePath <- determineAlsaCtlPath

  bracket (async $ alsaReaderThread mixerName effectivePath outputCallback mvar) cancel $ \a -> do

    -- Throw on this thread if there's an exception
    -- on the reader thread.
    link a

    cont $ takeMVar mvar

  where
    defaultPath = "/usr/sbin/alsactl"

    determineAlsaCtlPath =
      case alsaCtlPathOverride of
        Just path -> do
          found <- doesFileExist path
          if found
            then pure path
            else throwIO . ErrorCall $
                  "Specified alsactl file " ++ path ++ " does not exist"

        Nothing -> do
          (ec, path, err) <- readProcessWithExitCode "which" ["alsactl"] ""
          unless (null err) $ hPutStrLn stderr err
          case ec of
            ExitSuccess -> pure $ trimTrailingNewline path
            ExitFailure _ -> do
              found <- doesFileExist defaultPath
              if found
                then pure defaultPath
                else throwIO . ErrorCall $
                      "alsactl not found in PATH or at " ++
                      show defaultPath ++
                      "; please specify with --" ++
                      alsaCtlOptionName ++ "=/path/to/alsactl"


alsaReaderThread :: String -> String -> (String -> IO a) -> MVar () -> IO b
alsaReaderThread mixerName alsaCtlPath outputCallback mvar =
  let createProc = (proc "stdbuf" ["-oL", alsaCtlPath, "monitor", mixerName])
                      {std_out = CreatePipe}

      runAlsaOnce =
        withCreateProcess createProc $ \_ (Just alsaOut) _ _ -> do
          hSetBuffering alsaOut LineBuffering

          tryPutMVar mvar () -- Refresh immediately after restarting alsactl

          forever $ do
            c <- hGetChar alsaOut
            when (c == '\n') $
              -- This uses 'tryPutMVar' because 'putMVar' would make 'runVolume' run
              -- once for each event. But we want it to run only once after a burst
              -- of events.
              void $ tryPutMVar mvar ()
  in do
    limiter <- createRateLimiter alsaCtlRestartRateLimit

    forever $ do
      limiter

      catchJust
        (guard . isEOFError)
        runAlsaOnce
        pure

      outputCallback "Restarting alsactl..."



-- This is necessarily very inefficient on 'String's
trimTrailingNewline :: String -> String
trimTrailingNewline x =
  case reverse x of
    '\n' : '\r' : y -> reverse y
    '\n' : y -> reverse y
    _ -> x

-- |
-- Returns an IO action that completes at most once per @interval@.
-- The returned cation is not safe for concurrent use.
createRateLimiter :: NominalDiffTime -> IO (IO ())
createRateLimiter interval = do
  prevTimeRef <- newIORef Nothing

  let
    limiter = do
      prevTime0 <- readIORef prevTimeRef
      curTime <- getCurrentTime

      case prevTime0 of
        Just prevTime | diff <- interval - (curTime `diffUTCTime` prevTime),
                        diff > 0
                        -> do
                            threadDelayNDT diff
                            writeIORef prevTimeRef . Just =<< getCurrentTime

        _ -> writeIORef prevTimeRef (Just curTime)

  pure limiter

threadDelayNDT :: NominalDiffTime -> IO ()
threadDelayNDT ndt =
  threadDelay (round (realToFrac ndt * 1e6 :: Double))
