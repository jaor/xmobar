{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Localize
-- Copyright   :  (C) 2011 Martin Perner
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Martin Perner <martin@perner.cc>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides an interface to locale information e.g. for DateL
--
-----------------------------------------------------------------------------

module Localize
    ( setupTimeLocale,
      getTimeLocale
    ) where

import Foreign.C
import qualified System.Locale as L

#ifdef UTF8
import Codec.Binary.UTF8.String
#endif

--  get localized strings
type NlItem = CInt

#include <langinfo.h>
foreign import ccall unsafe "langinfo.h nl_langinfo"
  nl_langinfo :: NlItem -> IO CString

#{enum NlItem,
  , AM_STR , PM_STR \
  , D_T_FMT , D_FMT , T_FMT , T_FMT_AMPM \
  , ABDAY_1, ABDAY_7 \
  , DAY_1, DAY_7 \
  , ABMON_1, ABMON_12 \
  , MON_1, MON_12\
 }

getLangInfo :: NlItem -> IO String
getLangInfo item = do
  itemStr <- nl_langinfo item
#ifdef UTF8
  str <- peekCString itemStr
  return $ decodeString str
#else
  peekCString itemStr
#endif

#include <locale.h>
foreign import ccall unsafe "locale.h setlocale"
    setlocale :: CInt -> CString -> IO CString

setupTimeLocale :: String -> IO ()
setupTimeLocale l = withCString l (setlocale #const LC_TIME) >> return ()

getTimeLocale :: IO L.TimeLocale
getTimeLocale = do
  -- assumes that the defined values are increasing by exactly one.
  -- as they are defined consecutive in an enum this is reasonable
  days   <- mapM getLangInfo [day1 .. day7]
  abdays <- mapM getLangInfo [abday1 .. abday7]

  mons   <- mapM getLangInfo [mon1 .. mon12]
  abmons <- mapM getLangInfo [abmon1 .. abmon12]

  amstr <- getLangInfo amStr
  pmstr <- getLangInfo pmStr
  dtfmt <- getLangInfo dTFmt
  dfmt  <- getLangInfo dFmt
  tfmt  <- getLangInfo tFmt
  tfmta <- getLangInfo tFmtAmpm

  let t =  L.defaultTimeLocale {L.wDays  = zip days abdays
                               ,L.months = zip mons abmons
                               ,L.amPm = (amstr, pmstr)
                               ,L.dateTimeFmt = dtfmt
                               ,L.dateFmt = dfmt
                               ,L.timeFmt = tfmt
                               ,L.time12Fmt = tfmta}
  return t
