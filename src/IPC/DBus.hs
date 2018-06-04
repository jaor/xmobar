-----------------------------------------------------------------------------
-- |
-- Module      :  DBus
-- Copyright   :  (c) Jochen Keil
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jochen Keil <jochen dot keil at gmail dot com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- DBus IPC module for Xmobar
--
-----------------------------------------------------------------------------

module IPC.DBus (runIPC) where

import DBus
import DBus.Client hiding (interfaceName)
import qualified DBus.Client as DC
import Control.Monad (when)
import Control.Concurrent.STM
import Control.Exception (handle)
import System.IO (stderr, hPutStrLn)
import Control.Monad.IO.Class (liftIO)

import Signal

busName :: BusName
busName = busName_ "org.Xmobar.Control"

objectPath :: ObjectPath
objectPath = objectPath_ "/org/Xmobar/Control"

interfaceName :: InterfaceName
interfaceName = interfaceName_ "org.Xmobar.Control"

runIPC :: TMVar SignalType -> IO ()
runIPC mvst = handle printException exportConnection
    where
    printException :: ClientError -> IO ()
    printException = hPutStrLn stderr . clientErrorMessage
    exportConnection = do
        client <- connectSession
        requestName client busName [ nameDoNotQueue ]
        export client objectPath defaultInterface
          { DC.interfaceName = interfaceName
          , DC.interfaceMethods = [ sendSignalMethod mvst ]
          }

sendSignalMethod :: TMVar SignalType -> Method
sendSignalMethod mvst = makeMethod sendSignalName
    (signature_ [variantType $ toVariant (undefined :: SignalType)])
    (signature_ [])
    sendSignalMethodCall
    where
    sendSignalName :: MemberName
    sendSignalName = memberName_ "SendSignal"

    sendSignalMethodCall :: MethodCall -> DBusR Reply
    sendSignalMethodCall mc = liftIO $ do
        when ( methodCallMember mc == sendSignalName )
             $ mapM_ (sendSignal . fromVariant) (methodCallBody mc)
        return ( ReplyReturn [] )

    sendSignal :: Maybe SignalType -> IO ()
    sendSignal = maybe (return ()) (atomically . putTMVar mvst)
