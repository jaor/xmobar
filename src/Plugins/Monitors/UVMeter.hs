{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.UVMeter
-- Copyright   :  (c) Róman Joost
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Róman Joost
-- Stability   :  unstable
-- Portability :  unportable
--
-- An australian uv monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.UVMeter where

import Plugins.Monitors.Common

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.HTTP
import Text.Read (readMaybe)
import Text.XML
import Text.XML.Cursor


uvConfig :: IO MConfig
uvConfig = mkMConfig
       "<station>" -- template
       ["station"                               -- available replacements
       ]

data UvInfo = UV { index :: String }
    deriving (Show)

uvURL :: String
uvURL = "http://www.arpansa.gov.au/uvindex/realtime/xml/uvvalues.xml"

getData :: IO LT.Text
getData = do
    let request = getRequest uvURL
    body <- simpleHTTP request >>= getResponseBody
    return (LT.pack body)

textToXMLDocument :: LT.Text -> Cursor
textToXMLDocument txt = fromDocument doc
    where Right doc = parseText def txt

formatUVRating :: Maybe Float -> Monitor String
formatUVRating Nothing = getConfigValue naString
formatUVRating (Just x) = do
    uv <- showWithColors show x
    parseTemplate [uv]

getUVRating :: String -> Cursor -> Maybe Float
getUVRating stid cursor = readMaybe raw
    where raw = T.unpack $ T.concat $
                cursor $// element "location"
                    >=> attributeIs "id" (T.pack stid)
                    >=> child
                    >=> element "index"
                    &// content

runUVMeter :: [String] -> Monitor String
runUVMeter [] = return "N.A."
runUVMeter (s:_) = do
    resp <- io getData
    let doc = textToXMLDocument resp
    let uv = getUVRating s doc
    formatUVRating uv
