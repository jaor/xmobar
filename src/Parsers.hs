{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Parsers
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Parsers needed for Xmobar, a text based status bar
--
-----------------------------------------------------------------------------

module Parsers
    ( parseString
    , parseTemplate
    , parseConfig
    , Widget(..)
    ) where

import Config
import Runnable
import Commands
import Actions

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Perm
import Graphics.X11.Types (Button)

data Widget = Icon String | Text String

type ColorString = String

-- | Runs the string parser
parseString :: Config -> String -> IO [(Widget, ColorString, Maybe [Action])]
parseString c s =
    case parse (stringParser (fgColor c) Nothing) "" s of
      Left  _ -> return [(Text $ "Could not parse string: " ++ s
                          , fgColor c
                          , Nothing)]
      Right x -> return (concat x)

-- | Gets the string and combines the needed parsers
stringParser :: String -> Maybe [Action]
                -> Parser [[(Widget, ColorString, Maybe [Action])]]
stringParser c a = manyTill (textParser c a <|> try (iconParser c a) <|>
                             try (actionParser c a) <|> colorParser a) eof

-- | Parses a maximal string without color markup.
textParser :: String -> Maybe [Action]
              -> Parser [(Widget, ColorString, Maybe [Action])]
textParser c a = do s <- many1 $
                          noneOf "<" <|>
                            try (notFollowedBy' (char '<')
                                  (try (string "fc=")  <|>
                                   try (string "action=") <|>
                                   try (string "/action>") <|>
                                   try (string "icon=") <|>
                                   string "/fc>"))
                    return [(Text s, c, a)]

-- | Wrapper for notFollowedBy that returns the result of the first parser.
--   Also works around the issue that, at least in Parsec 3.0.0, notFollowedBy
--   accepts only parsers with return type Char.
notFollowedBy' :: Parser a -> Parser b -> Parser a
notFollowedBy' p e = do x <- p
                        notFollowedBy $ try (e >> return '*')
                        return x

iconParser :: String -> Maybe [Action]
              -> Parser [(Widget, ColorString, Maybe [Action])]
iconParser c a = do
  string "<icon="
  i <- manyTill (noneOf ">") (try (string "/>"))
  return [(Icon i, c, a)]

actionParser :: String -> Maybe [Action] -> Parser [(Widget, ColorString, Maybe [Action])]
actionParser c act = do
  string "<action="
  command <- choice [between (char '`') (char '`') (many1 (noneOf "`")),
                   many1 (noneOf ">")]
  buttons <- (char '>' >> return "1") <|> (space >> spaces >>
    between (string "button=") (string ">") (many1 (oneOf "12345")))
  let a = Spawn (toButtons buttons) command
      a' = case act of
        Nothing -> Just [a]
        Just act' -> Just $ a : act'
  s <- manyTill (try (textParser c a') <|> try (iconParser c a') <|>
                 try (colorParser a') <|> actionParser c a')
                (try $ string "</action>")
  return (concat s)

toButtons :: String -> [Button]
toButtons s = map (\x -> read [x]) s

-- | Parsers a string wrapped in a color specification.
colorParser :: Maybe [Action] -> Parser [(Widget, ColorString, Maybe [Action])]
colorParser a = do
  c <- between (string "<fc=") (string ">") colors
  s <- manyTill (try (textParser c a) <|> try (iconParser c a) <|>
                 try (colorParser a) <|> actionParser c a) (try $ string "</fc>")
  return (concat s)

-- | Parses a color specification (hex or named)
colors :: Parser String
colors = many1 (alphaNum <|> char ',' <|> char '#')

-- | Parses the output template string
templateStringParser :: Config -> Parser (String,String,String)
templateStringParser c = do
  s   <- allTillSep c
  com <- templateCommandParser c
  ss  <- allTillSep c
  return (com, s, ss)

-- | Parses the command part of the template string
templateCommandParser :: Config -> Parser String
templateCommandParser c =
  let chr = char . head . sepChar
  in  between (chr c) (chr c) (allTillSep c)

-- | Combines the template parsers
templateParser :: Config -> Parser [(String,String,String)]
templateParser = many . templateStringParser

-- | Actually runs the template parsers
parseTemplate :: Config -> String -> IO [(Runnable,String,String)]
parseTemplate c s =
    do str <- case parse (templateParser c) "" s of
                Left _  -> return [("", s, "")]
                Right x -> return x
       let cl = map alias (commands c)
           m  = Map.fromList $ zip cl (commands c)
       return $ combine c m str

-- | Given a finite "Map" and a parsed template produce the resulting
-- output string.
combine :: Config -> Map.Map String Runnable
           -> [(String, String, String)] -> [(Runnable,String,String)]
combine _ _ [] = []
combine c m ((ts,s,ss):xs) = (com, s, ss) : combine c m xs
    where com  = Map.findWithDefault dflt ts m
          dflt = Run $ Com ts [] [] 10

allTillSep :: Config -> Parser String
allTillSep = many . noneOf . sepChar

stripComments :: String -> String
stripComments =
  unlines . map (drop 5 . strip False . (replicate 5 ' '++)) . lines
    where strip m ('-':'-':xs) = if m then "--" ++ strip m xs else ""
          strip m ('\\':xss) = case xss of
                                '\\':xs -> '\\' : strip m xs
                                _ -> strip m $ drop 1 xss
          strip m ('"':xs) = '"': strip (not m) xs
          strip m (x:xs) = x : strip m xs
          strip _ [] = []

-- | Parse the config, logging a list of fields that were missing and replaced
-- by the default definition.
parseConfig :: String -> Either ParseError (Config,[String])
parseConfig = runParser parseConf fields "Config" . stripComments
    where
      parseConf = do
        many space
        sepEndSpc ["Config","{"]
        x <- perms
        eof
        s <- getState
        return (x,s)

      perms = permute $ Config
              <$?> pFont <|?> pBgColor <|?> pFgColor <|?> pPosition
              <|?> pBorder <|?> pBdColor <|?> pHideOnStart <|?> pAllDesktops
              <|?> pOverrideRedirect <|?> pLowerOnStart <|?> pPersistent
              <|?> pCommands <|?> pSepChar <|?> pAlignSep <|?> pTemplate

      fields    = [ "font", "bgColor", "fgColor", "sepChar", "alignSep"
                  , "border", "borderColor" ,"template", "position"
                  , "allDesktops", "overrideRedirect"
                  , "hideOnStart", "lowerOnStart", "persistent", "commands"
                  ]

      pFont = strField font "font"
      pBgColor = strField bgColor "bgColor"
      pFgColor = strField fgColor "fgColor"
      pBdColor = strField borderColor "borderColor"
      pSepChar = strField sepChar "sepChar"
      pAlignSep = strField alignSep "alignSep"
      pTemplate = strField template "template"

      pPosition = readField position "position"
      pHideOnStart = readField hideOnStart "hideOnStart"
      pLowerOnStart = readField lowerOnStart "lowerOnStart"
      pPersistent = readField persistent "persistent"
      pBorder = readField border "border"
      pAllDesktops = readField allDesktops "allDesktops"
      pOverrideRedirect = readField overrideRedirect "overrideRedirect"

      pCommands = field commands "commands" readCommands

      staticPos = do string "Static"
                     wrapSkip (string "{")
                     p <- many (noneOf "}")
                     wrapSkip (string "}")
                     string ","
                     return ("Static {"  ++ p  ++ "}")
      tillFieldEnd = staticPos <|> many (noneOf ",}\n\r")

      commandsEnd  = wrapSkip (string "]") >> (string "}" <|> notNextRun)
      notNextRun = do {string ","
                      ; notFollowedBy $ wrapSkip $ string "Run"
                      ; return ","
                      }
      readCommands = manyTill anyChar (try commandsEnd) >>=
                        read' commandsErr . flip (++) "]"

      strField e n = field e n . between (strDel "start" n) (strDel "end" n) .
                     many $ noneOf "\"\n\r"
      strDel t n = char '"' <?> strErr t n
      strErr t n = "the " ++ t ++ " of the string field " ++ n ++
                       " - a double quote (\")."

      wrapSkip   x = many space >> x >>= \r -> many space >> return r
      sepEndSpc    = mapM_ (wrapSkip . try . string)
      fieldEnd     = many $ space <|> oneOf ",}"
      field  e n c = (,) (e defaultConfig) $
                     updateState (filter (/= n)) >> sepEndSpc [n,"="] >>
                     wrapSkip c >>= \r -> fieldEnd >> return r
      readField a n = field a n $ tillFieldEnd >>= read' n
      read' d s = case reads s of
                    [(x, _)] -> return x
                    _ -> fail $ "error reading the " ++ d ++ " field: " ++ s

commandsErr :: String
commandsErr = "commands: this usually means that a command could not" ++
              "\nbe parsed." ++
              "\nThe error could be located at the begining of the command" ++
              "\nwhich follows the offending one."
