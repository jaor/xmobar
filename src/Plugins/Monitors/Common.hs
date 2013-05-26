-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Common
-- Copyright   :  (c) 2010, 2011, 2013 Jose Antonio Ortega Ruiz
--                (c) 2007-2010 Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Utilities used by xmobar's monitors
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Common (
                       -- * Monitors
                       -- $monitor
                         Monitor
                       , MConfig (..)
                       , Opts (..)
                       , setConfigValue
                       , getConfigValue
                       , mkMConfig
                       , runM
                       , runMB
                       , io
                       -- * Parsers
                       -- $parsers
                       , runP
                       , skipRestOfLine
                       , getNumbers
                       , getNumbersAsString
                       , getAllBut
                       , getAfterString
                       , skipTillString
                       , parseTemplate
                       , parseTemplate'
                       -- ** String Manipulation
                       -- $strings
                       , padString
                       , showWithPadding
                       , showWithColors
                       , showWithColors'
                       , showPercentWithColors
                       , showPercentsWithColors
                       , showPercentBar
                       , showLogBar
                       , showWithUnits
                       , takeDigits
                       , showDigits
                       , floatToPercent
                       , parseFloat
                       , parseInt
                       , stringParser
                       ) where


import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as B
import Data.IORef
import qualified Data.Map as Map
import Data.List
import Numeric
import Text.ParserCombinators.Parsec
import System.Console.GetOpt
import Control.Exception (SomeException,handle)

import Plugins
-- $monitor

type Monitor a = ReaderT MConfig IO a

data MConfig =
    MC { normalColor :: IORef (Maybe String)
       , low         :: IORef Int
       , lowColor    :: IORef (Maybe String)
       , high        :: IORef Int
       , highColor   :: IORef (Maybe String)
       , template    :: IORef String
       , export      :: IORef [String]
       , ppad        :: IORef Int
       , decDigits   :: IORef Int
       , minWidth    :: IORef Int
       , maxWidth    :: IORef Int
       , padChars    :: IORef String
       , padRight    :: IORef Bool
       , barBack     :: IORef String
       , barFore     :: IORef String
       , barWidth    :: IORef Int
       , useSuffix   :: IORef Bool
       }

-- | from 'http:\/\/www.haskell.org\/hawiki\/MonadState'
type Selector a = MConfig -> IORef a

sel :: Selector a -> Monitor a
sel s =
    do hs <- ask
       liftIO $ readIORef (s hs)

mods :: Selector a -> (a -> a) -> Monitor ()
mods s m =
    do v <- ask
       io $ modifyIORef (s v) m

setConfigValue :: a -> Selector a -> Monitor ()
setConfigValue v s =
       mods s (\_ -> v)

getConfigValue :: Selector a -> Monitor a
getConfigValue = sel

mkMConfig :: String
          -> [String]
          -> IO MConfig
mkMConfig tmpl exprts =
    do lc <- newIORef Nothing
       l  <- newIORef 33
       nc <- newIORef Nothing
       h  <- newIORef 66
       hc <- newIORef Nothing
       t  <- newIORef tmpl
       e  <- newIORef exprts
       p  <- newIORef 0
       d  <- newIORef 0
       mn <- newIORef 0
       mx <- newIORef 0
       pc <- newIORef " "
       pr <- newIORef False
       bb <- newIORef ":"
       bf <- newIORef "#"
       bw <- newIORef 10
       up <- newIORef False
       return $ MC nc l lc h hc t e p d mn mx pc pr bb bf bw up

data Opts = HighColor String
          | NormalColor String
          | LowColor String
          | Low String
          | High String
          | Template String
          | PercentPad String
          | DecDigits String
          | MinWidth String
          | MaxWidth String
          | Width String
          | PadChars String
          | PadAlign String
          | BarBack String
          | BarFore String
          | BarWidth String
          | UseSuffix String

options :: [OptDescr Opts]
options =
    [
      Option "H"  ["High"] (ReqArg High "number") "The high threshold"
    , Option "L"  ["Low"] (ReqArg Low "number") "The low threshold"
    , Option "h"  ["high"] (ReqArg HighColor "color number") "Color for the high threshold: ex \"#FF0000\""
    , Option "n"  ["normal"] (ReqArg NormalColor "color number") "Color for the normal threshold: ex \"#00FF00\""
    , Option "l"  ["low"] (ReqArg LowColor "color number") "Color for the low threshold: ex \"#0000FF\""
    , Option "t"  ["template"] (ReqArg Template "output template") "Output template."
    , Option "S"  ["suffix"] (ReqArg UseSuffix "True/False") "Use % to display percents or other suffixes."
    , Option "d"  ["ddigits"] (ReqArg DecDigits "decimal digits") "Number of decimal digits to display."
    , Option "p"  ["ppad"] (ReqArg PercentPad "percent padding") "Minimum percentage width."
    , Option "m"  ["minwidth"] (ReqArg MinWidth "minimum width") "Minimum field width"
    , Option "M"  ["maxwidth"] (ReqArg MaxWidth "maximum width") "Maximum field width"
    , Option "w"  ["width"] (ReqArg Width "fixed width") "Fixed field width"
    , Option "c"  ["padchars"] (ReqArg PadChars "padding chars") "Characters to use for padding"
    , Option "a"  ["align"] (ReqArg PadAlign "padding alignment") "'l' for left padding, 'r' for right"
    , Option "b"  ["bback"] (ReqArg BarBack "bar background") "Characters used to draw bar backgrounds"
    , Option "f"  ["bfore"] (ReqArg BarFore "bar foreground") "Characters used to draw bar foregrounds"
    , Option "W"  ["bwidth"] (ReqArg BarWidth "bar width") "Bar width"
    ]

doArgs :: [String] -> ([String] -> Monitor String) -> Monitor String
doArgs args action =
    case getOpt Permute options args of
      (o, n, [])   -> do doConfigOptions o
                         action n
      (_, _, errs) -> return (concat errs)

doConfigOptions :: [Opts] -> Monitor ()
doConfigOptions [] = io $ return ()
doConfigOptions (o:oo) =
    do let next = doConfigOptions oo
           nz s = let x = read s in max 0 x
           bool = (`elem` ["True", "true", "Yes", "yes", "On", "on"])
       (case o of
          High        h -> setConfigValue (read h) high
          Low         l -> setConfigValue (read l) low
          HighColor   c -> setConfigValue (Just c) highColor
          NormalColor c -> setConfigValue (Just c) normalColor
          LowColor    c -> setConfigValue (Just c) lowColor
          Template    t -> setConfigValue t template
          PercentPad  p -> setConfigValue (nz p) ppad
          DecDigits   d -> setConfigValue (nz d) decDigits
          MinWidth    w -> setConfigValue (nz w) minWidth
          MaxWidth    w -> setConfigValue (nz w) maxWidth
          Width       w -> setConfigValue (nz w) minWidth >>
                           setConfigValue (nz w) maxWidth
          PadChars    s -> setConfigValue s padChars
          PadAlign    a -> setConfigValue ("r" `isPrefixOf` a) padRight
          BarBack     s -> setConfigValue s barBack
          BarFore     s -> setConfigValue s barFore
          BarWidth    w -> setConfigValue (nz w) barWidth
          UseSuffix   u -> setConfigValue (bool u) useSuffix) >> next

runM :: [String] -> IO MConfig -> ([String] -> Monitor String) -> Int
        -> (String -> IO ()) -> IO ()
runM args conf action r = runMB args conf action (tenthSeconds r)

runMB :: [String] -> IO MConfig -> ([String] -> Monitor String)
         -> IO () -> (String -> IO ()) -> IO ()
runMB args conf action wait cb = handle (cb . showException) loop
  where ac = doArgs args action
        loop = conf >>= runReaderT ac >>= cb >> wait >> loop

showException :: SomeException -> String
showException = ("error: "++) . show . flip asTypeOf undefined

io :: IO a -> Monitor a
io = liftIO

-- $parsers

runP :: Parser [a] -> String -> IO [a]
runP p i =
    case parse p "" i of
      Left _ -> return []
      Right x  -> return x

getAllBut :: String -> Parser String
getAllBut s =
    manyTill (noneOf s) (char $ head s)

getNumbers :: Parser Float
getNumbers = skipMany space >> many1 digit >>= \n -> return $ read n

getNumbersAsString :: Parser String
getNumbersAsString = skipMany space >> many1 digit >>= \n -> return n

skipRestOfLine :: Parser Char
skipRestOfLine =
    do many $ noneOf "\n\r"
       newline

getAfterString :: String -> Parser String
getAfterString s =
    do { try $ manyTill skipRestOfLine $ string s
       ; manyTill anyChar newline
       } <|> return ""

skipTillString :: String -> Parser String
skipTillString s =
    manyTill skipRestOfLine $ string s

-- | Parses the output template string
templateStringParser :: Parser (String,String,String)
templateStringParser =
    do { s <- nonPlaceHolder
       ; com <- templateCommandParser
       ; ss <- nonPlaceHolder
       ; return (s, com, ss)
       }
    where
      nonPlaceHolder = liftM concat . many $
                       many1 (noneOf "<") <|> colorSpec <|> iconSpec

-- | Recognizes color specification and returns it unchanged
colorSpec :: Parser String
colorSpec = try (string "</fc>") <|> try (
            do string "<fc="
               s <- many1 (alphaNum <|> char ',' <|> char '#')
               char '>'
               return $ "<fc=" ++ s ++ ">")

-- | Recognizes icon specification and returns it unchanged
iconSpec :: Parser String
iconSpec = try (do string "<icon="
                   i <- manyTill (noneOf ">") (try (string "/>"))
                   return $ "<icon=" ++ i ++ "/>")

-- | Parses the command part of the template string
templateCommandParser :: Parser String
templateCommandParser =
    do { char '<'
       ; com <- many $ noneOf ">"
       ; char '>'
       ; return com
       }

-- | Combines the template parsers
templateParser :: Parser [(String,String,String)]
templateParser = many templateStringParser --"%")

-- | Takes a list of strings that represent the values of the exported
-- keys. The strings are joined with the exported keys to form a map
-- to be combined with 'combine' to the parsed template. Returns the
-- final output of the monitor.
parseTemplate :: [String] -> Monitor String
parseTemplate l =
    do t <- getConfigValue template
       e <- getConfigValue export
       let m = Map.fromList . zip e $ l
       parseTemplate' t m

-- | Parses the template given to it with a map of export values and combines
-- them
parseTemplate' :: String -> Map.Map String String -> Monitor String
parseTemplate' t m =
    do s <- io $ runP templateParser t
       combine m s

-- | Given a finite "Map" and a parsed template t produces the
-- | resulting output string as the output of the monitor.
combine :: Map.Map String String -> [(String, String, String)] -> Monitor String
combine _ [] = return []
combine m ((s,ts,ss):xs) =
    do next <- combine m xs
       let str = Map.findWithDefault err ts m
           err = "<" ++ ts ++ " not found!>"
       nstr <- parseTemplate' str m
       return $ s ++ (if null nstr then str else nstr) ++ ss ++ next

-- $strings

type Pos = (Int, Int)

takeDigits :: Int -> Float -> Float
takeDigits d n =
    fromIntegral (round (n * fact) :: Int) / fact
  where fact = 10 ^ d

showDigits :: (RealFloat a) => Int -> a -> String
showDigits d n = showFFloat (Just d) n ""

showWithUnits :: Int -> Int -> Float -> String
showWithUnits d n x
  | x < 0 = '-' : showWithUnits d n (-x)
  | n > 3 || x < 10^(d + 1) = show (round x :: Int) ++ units n
  | x <= 1024 = showDigits d (x/1024) ++ units (n+1)
  | otherwise = showWithUnits d (n+1) (x/1024)
  where units = (!!) ["B", "K", "M", "G", "T"]

padString :: Int -> Int -> String -> Bool -> String -> String
padString mnw mxw pad pr s =
  let len = length s
      rmin = if mnw <= 0 then 1 else mnw
      rmax = if mxw <= 0 then max len rmin else mxw
      (rmn, rmx) = if rmin <= rmax then (rmin, rmax) else (rmax, rmin)
      rlen = min (max rmn len) rmx
  in if rlen < len then
       take rlen s
     else let ps = take (rlen - len) (cycle pad)
          in if pr then s ++ ps else ps ++ s

parseFloat :: String -> Float
parseFloat s = case readFloat s of
  (v, _):_ -> v
  _ -> 0

parseInt :: String -> Int
parseInt s = case readDec s of
  (v, _):_ -> v
  _ -> 0

floatToPercent :: Float -> Monitor String
floatToPercent n =
  do pad <- getConfigValue ppad
     pc <- getConfigValue padChars
     pr <- getConfigValue padRight
     up <- getConfigValue useSuffix
     let p = showDigits 0 (n * 100)
         ps = if up then "%" else ""
     return $ padString pad pad pc pr p ++ ps

stringParser :: Pos -> B.ByteString -> String
stringParser (x,y) =
     B.unpack . li x . B.words . li y . B.lines
    where li i l | length l > i = l !! i
                 | otherwise    = B.empty

setColor :: String -> Selector (Maybe String) -> Monitor String
setColor str s =
    do a <- getConfigValue s
       case a of
            Nothing -> return str
            Just c -> return $
                "<fc=" ++ c ++ ">" ++ str ++ "</fc>"

showWithPadding :: String -> Monitor String
showWithPadding s =
    do mn <- getConfigValue minWidth
       mx <- getConfigValue maxWidth
       p <- getConfigValue padChars
       pr <- getConfigValue padRight
       return $ padString mn mx p pr s

colorizeString :: (Num a, Ord a) => a -> String -> Monitor String
colorizeString x s = do
    h <- getConfigValue high
    l <- getConfigValue low
    let col = setColor s
        [ll,hh] = map fromIntegral $ sort [l, h] -- consider high < low
    head $ [col highColor   | x > hh ] ++
           [col normalColor | x > ll ] ++
           [col lowColor    | True]

showWithColors :: (Num a, Ord a) => (a -> String) -> a -> Monitor String
showWithColors f x = showWithPadding (f x) >>= colorizeString x

showWithColors' :: (Num a, Ord a) => String -> a -> Monitor String
showWithColors' str = showWithColors (const str)

showPercentsWithColors :: [Float] -> Monitor [String]
showPercentsWithColors fs =
  do fstrs <- mapM floatToPercent fs
     zipWithM (showWithColors . const) fstrs (map (*100) fs)

showPercentWithColors :: Float -> Monitor String
showPercentWithColors f = liftM head $ showPercentsWithColors [f]

showPercentBar :: Float -> Float -> Monitor String
showPercentBar v x = do
  bb <- getConfigValue barBack
  bf <- getConfigValue barFore
  bw <- getConfigValue barWidth
  let len = min bw $ round (fromIntegral bw * x)
  s <- colorizeString v (take len $ cycle bf)
  return $ s ++ take (bw - len) (cycle bb)

showLogBar :: Float -> Float -> Monitor String
showLogBar f v = do
  h <- fromIntegral `fmap` getConfigValue high
  l <- fromIntegral `fmap` getConfigValue low
  bw <- fromIntegral `fmap` getConfigValue barWidth
  let [ll, hh] = sort [l, h]
      choose x | x == 0.0 = 0
               | x <= ll = 1 / bw
               | otherwise = f + logBase 2 (x / hh) / bw
  showPercentBar v $ choose v
