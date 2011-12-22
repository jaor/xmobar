{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Mpris
-- Copyright   :  (c) Artem Tarasov
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Artem Tarasov <lomereiter@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
--   MPRIS song info
--
----------------------------------------------------------------------------

module Plugins.Monitors.Mpris ( mprisConfig, runMPRIS1, runMPRIS2 ) where

-- TODO: listen to signals

import Plugins.Monitors.Common

import Text.Printf (printf)
import qualified DBus.Client.Simple as C
import DBus.Types
import DBus.Connection ( ConnectionError )
import Data.Maybe ( fromJust )
import Data.Int ( Int32, Int64 )
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T

import Control.Exception (try, evaluate)

class MprisVersion a where
    getProxy :: a -> C.Client -> String -> IO C.Proxy
    getMetadataReply :: a -> C.Client -> String -> IO [Variant]
    fieldsList :: a -> [String]

data MprisVersion1 = MprisVersion1
instance MprisVersion MprisVersion1 where
    getProxy MprisVersion1 c p = do
        let playerBusName = T.concat ["org.mpris.", T.pack p]
        C.proxy c (C.busName_ playerBusName) "/Player"
    getMetadataReply MprisVersion1 c p = do
        player <- getProxy MprisVersion1 c p
        C.call player "org.freedesktop.MediaPlayer" "GetMetadata" []
    fieldsList MprisVersion1 = [ "album", "artist", "arturl", "mtime", "title", "tracknumber" ]

data MprisVersion2 = MprisVersion2
instance MprisVersion MprisVersion2 where
    getProxy MprisVersion2 c p = do
        let playerBusName = T.concat ["org.mpris.MediaPlayer2.", T.pack p]
        C.proxy c (C.busName_ playerBusName) "/org/mpris/MediaPlayer2"
    getMetadataReply MprisVersion2 c p = do
        player <- getProxy MprisVersion2 c p
        C.call player "org.freedesktop.DBus.Properties" 
                      "Get" 
                      (map (toVariant::String -> Variant) 
                           ["org.mpris.MediaPlayer2.Player", "Metadata"]
                      )
    fieldsList MprisVersion2 = [ "xesam:album", "xesam:artist", "mpris:artUrl"
                               , "mpris:length", "xesam:title", "xesam:trackNumber"
                               ]

mprisConfig :: IO MConfig
mprisConfig = mkMConfig "<artist> - <title>"
                [ "album", "artist", "arturl", "length" , "title", "tracknumber" 
                ]

dbusClient :: C.Client
dbusClient = unsafePerformIO C.connectSession

runMPRIS :: (MprisVersion a) => a -> String -> [String] -> Monitor String
runMPRIS version playerName _ = do
    metadata <- io $ getMetadata version dbusClient playerName
    parseTemplate $ makeList version metadata

runMPRIS1 :: String -> [String] -> Monitor String
runMPRIS1 = runMPRIS MprisVersion1

runMPRIS2 :: String -> [String] -> Monitor String
runMPRIS2 = runMPRIS MprisVersion2

---------------------------------------------------------------------------

fromVar :: (IsVariant a) => Variant -> a
fromVar = fromJust . fromVariant

unpackMetadata :: [Variant] -> [(String, Variant)]
unpackMetadata [] = []
unpackMetadata xs = ((map (\(k, v) -> (fromVar k, fromVar v))) . unpack . head) xs where 
                      unpack v = case variantType v of
                            TypeDictionary _ _ -> dictionaryItems $ fromVar v
                            TypeVariant -> unpack $ fromVar v
                            TypeStructure _ -> unpack $ head $ structureItems $ fromVar v
                            _ -> []

getMetadata :: (MprisVersion a) => a -> C.Client -> String -> IO [(String, Variant)]
getMetadata version client player = do
    reply <- try (getMetadataReply version client player) ::
                            IO (Either ConnectionError [Variant])
    return $ case reply of 
                  Right metadata -> unpackMetadata metadata; 
                  Left _ -> []

makeList :: (MprisVersion a) => a -> [(String, Variant)] -> [String]
makeList version md = map getStr (fieldsList version) where 
            formatTime n = (if hh == 0 then printf "%02d:%02d"
                                       else printf "%d:%02d:%02d" hh) mm ss
                           where hh = (n `div` 60) `div` 60
                                 mm = (n `div` 60) `mod` 60
                                 ss = n `mod` 60
            getStr str = case lookup str md of
                Nothing -> ""
                Just v -> case variantType v of
                            TypeString -> fromVar v
                            TypeInt32 -> let num = fromVar v in
                                          case str of
                                           "mtime" -> formatTime (num `div` 1000)
                                           "tracknumber" -> printf "%02d" num
                                           "mpris:length" -> formatTime (num `div` 1000000)
                                           "xesam:trackNumber" -> printf "%02d" num
                                           _ -> (show::Int32 -> String) num
                            TypeInt64 -> let num = fromVar v in
                                          case str of
                                           "mpris:length" -> formatTime (num `div` 1000000)
                                           _ -> (show::Int64 -> String) num
                            TypeArray TypeString -> fromVar $ head $ arrayItems $ fromVar v
                            _ -> ""
