{-# LANGUAGE CPP #-}
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
#if MIN_VERSION_dbus(1,0,0)
import DBus.Client hiding (interfaceName)
import qualified DBus.Client as D (interfaceName)
#else
import DBus.Client
#endif
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM
import Control.Exception (handle)
import System.IO (stderr, hPutStrLn)

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
        export client objectPath $ sendSignalInterface mvst

sendSignalInterface :: TMVar SignalType -> Interface
sendSignalInterface mvst = defaultInterface
        { D.interfaceName = interfaceName
        , interfaceMethods = [sendSignalMethod]
        }
    where
    sendSignalName :: MemberName
    sendSignalName = memberName_ "SendSignal"

    sendSignalMethodCall :: MethodCall -> DBusR Reply
    sendSignalMethodCall mc = do
        when ( methodCallMember mc == sendSignalName )
             $ mapM_ (sendSignal . fromVariant) (methodCallBody mc)
        return ( ReplyReturn [] )

    sendSignal :: Maybe SignalType -> DBusR ()
    sendSignal = maybe (return ()) (lift . atomically . putTMVar mvst)

    sendSignalMethod :: Method
    sendSignalMethod = makeMethod sendSignalName
        (signature_ [variantType $ toVariant (undefined :: SignalType)])
        (signature_ [])
        sendSignalMethodCall
