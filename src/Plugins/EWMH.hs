{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP, NamedFieldPuns, GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.EWMH
-- Copyright   :  (c) Spencer Janssen
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <spencerjanssen@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- An experimental plugin to display EWMH pager information
--
-----------------------------------------------------------------------------

module Plugins.EWMH (EWMH(..)) where

import Control.Monad.State
import Control.Monad.Reader
import Graphics.X11 hiding (Modifier, Color)
import Graphics.X11.Xlib.Extras
import Plugins
#ifdef UTF8
#undef UTF8
import Codec.Binary.UTF8.String as UTF8
#define UTF8
#endif
import Foreign.C (CChar, CLong)
import XUtil (nextEvent')

import Data.List (intersperse, intercalate)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


data EWMH = EWMH | EWMHFMT Component deriving (Read, Show)

instance Exec EWMH where
    alias EWMH = "EWMH"

    start ew cb = allocaXEvent $ \ep -> execM $ do
        d <- asks display
        r <- asks root

        liftIO xSetErrorHandler

        liftIO $ selectInput d r propertyChangeMask
        handlers' <- mapM (\(a, h) -> liftM2 (,) (getAtom a) (return h)) handlers
        mapM_ ((=<< asks root) . snd) handlers'

        forever $ do
            liftIO . cb . fmtOf ew =<< get
            liftIO $ nextEvent' d ep
            e <- liftIO $ getEvent ep
            case e of
                PropertyEvent { ev_atom = a, ev_window = w } ->
                    case lookup a handlers' of
                        Just f -> f w
                        _      -> return ()
                _ -> return ()

        return ()

defaultPP = Sep (Text " : ") [ Workspaces [Color "white" "black" :% Current, Hide :% Empty]
                             , Layout
                             , Color "#00ee00" "" :$ Short 120 :$ WindowName]

fmtOf EWMH = flip fmt defaultPP
fmtOf (EWMHFMT f) = flip fmt f

sep :: [a] -> [[a]] -> [a]
sep x xs = intercalate x $ filter (not . null) xs

fmt :: EwmhState -> Component -> String
fmt e (Text s) = s
fmt e (l :+ r) = fmt e l ++ fmt e r
fmt e (m :$ r) = modifier m $ fmt e r
fmt e (Sep c xs) = sep (fmt e c) $ map (fmt e) xs
fmt e WindowName = windowName $ Map.findWithDefault initialClient (activeWindow e) (clients e)
fmt e Layout = layout e
fmt e (Workspaces opts) = sep " "
                            [foldr ($) n [modifier m | (m :% a) <- opts, a `elem` as]
                                | (n, as) <- attrs]
 where
    stats i = [ (Current, i == currentDesktop e)
              , (Empty, Set.notMember i nonEmptys && i /= currentDesktop e)
              -- TODO for visible , (Visibl
              ]
    attrs :: [(String, [WsType])]
    attrs = [(n, [s | (s, b) <- stats i, b]) | (i, n) <- zip [0 ..] (desktopNames e)]
    nonEmptys = Set.unions . map desktops . Map.elems $ clients e

modifier :: Modifier -> String -> String
modifier Hide = const ""
modifier (Color fg bg) = \x -> concat ["<fc=", fg, if null bg then "" else "," ++ bg
                                      , ">", x, "</fc>"]
modifier (Short n) = take n
modifier (Wrap l r) = \x -> l ++ x ++ r

data Component = Text String
               | Component :+ Component
               | Modifier :$ Component
               | Sep Component [Component]
               | WindowName
               | Layout
               | Workspaces [WsOpt]
    deriving (Read, Show)

infixr 0 :$
infixr 5 :+

data Modifier = Hide
              | Color String String
              | Short Int
              | Wrap String String
    deriving (Read, Show)

data WsOpt = Modifier :% WsType
           | WSep Component
    deriving (Read, Show)
infixr 0 :%

data WsType = Current | Empty | Visible
    deriving (Read, Show, Eq)

data EwmhConf  = C { root :: Window
                   , display :: Display }

data EwmhState = S { currentDesktop :: CLong
                   , activeWindow :: Window
                   , desktopNames :: [String]
                   , layout :: String
                   , clients :: Map Window Client }
    deriving Show

data Client = Cl { windowName :: String
                 , desktops :: Set CLong }
    deriving Show

getAtom :: String -> M Atom
getAtom s = do
    d <- asks display
    liftIO $ internAtom d s False

windowProperty32 :: String -> Window -> M (Maybe [CLong])
windowProperty32 s w = do
    (C {display}) <- ask
    a <- getAtom s
    liftIO $ getWindowProperty32 display a w

windowProperty8 :: String -> Window -> M (Maybe [CChar])
windowProperty8 s w = do
    (C {display}) <- ask
    a <- getAtom s
    liftIO $ getWindowProperty8 display a w

initialState :: EwmhState
initialState = S 0 0 [] [] Map.empty

initialClient :: Client
initialClient = Cl "" Set.empty

handlers, clientHandlers :: [(String, Updater)]
handlers = [ ("_NET_CURRENT_DESKTOP", updateCurrentDesktop)
           , ("_NET_DESKTOP_NAMES", updateDesktopNames )
           , ("_NET_ACTIVE_WINDOW", updateActiveWindow)
           , ("_NET_CLIENT_LIST", updateClientList)
           ] ++ clientHandlers

clientHandlers = [ ("_NET_WM_NAME", updateName)
                 , ("_NET_WM_DESKTOP", updateDesktop) ]

newtype M a = M (ReaderT EwmhConf (StateT EwmhState IO) a)
    deriving (Monad, Functor, MonadIO, MonadReader EwmhConf, MonadState EwmhState)

execM :: M a -> IO a
execM (M m) = do
    d <- openDisplay ""
    r <- rootWindow d (defaultScreen d)
    let conf = C r d
    evalStateT (runReaderT m (C r d)) initialState

type Updater = Window -> M ()

updateCurrentDesktop, updateDesktopNames, updateActiveWindow :: Updater
updateCurrentDesktop _ = do
    (C {root}) <- ask
    mwp <- windowProperty32 "_NET_CURRENT_DESKTOP" root
    case mwp of
        Just [x] -> modify (\s -> s { currentDesktop = x })
        _        -> return ()

updateActiveWindow _ = do
    (C {root}) <- ask
    mwp <- windowProperty32 "_NET_ACTIVE_WINDOW" root
    case mwp of
        Just [x] -> modify (\s -> s { activeWindow = fromIntegral x })
        _        -> return ()

updateDesktopNames _ = do
    (C {root}) <- ask
    mwp <- windowProperty8 "_NET_DESKTOP_NAMES" root
    case mwp of
        Just xs -> modify (\s -> s { desktopNames = parse xs })
        _       -> return ()
 where
    dropNull ('\0':xs) = xs
    dropNull xs        = xs

    split []        = []
    split xs        = case span (/= '\0') xs of
                        (x, ys) -> x : split (dropNull ys)
    parse = split . decodeCChar

updateClientList _ = do
    (C {root}) <- ask
    mwp <- windowProperty32 "_NET_CLIENT_LIST" root
    case mwp of
        Just xs -> do
                    cl <- gets clients
                    let cl' = Map.fromList $ map (flip (,) initialClient . fromIntegral) xs
                        dels = Map.difference cl cl'
                        new = Map.difference cl' cl
                    modify (\s -> s { clients = Map.union (Map.intersection cl cl') cl'})
                    mapM_ (unmanage . fst) (Map.toList dels)
                    mapM_ (listen . fst)   (Map.toList cl')
                    mapM_ (update . fst)   (Map.toList new)
        _       -> return ()
 where
    unmanage w = asks display >>= \d -> liftIO $ selectInput d w 0
    listen w = asks display >>= \d -> liftIO $ selectInput d w propertyChangeMask
    update w = mapM_ (($ w) . snd) clientHandlers

modifyClient :: Window -> (Client -> Client) -> M ()
modifyClient w f = modify (\s -> s { clients = Map.alter f' w $ clients s })
 where
    f' Nothing  = Just $ f initialClient
    f' (Just x) = Just $ f x

updateName w = do
    mwp <- windowProperty8 "_NET_WM_NAME" w
    case mwp of
        Just xs -> modifyClient w (\c -> c { windowName = decodeCChar xs })
        _       -> return ()

updateDesktop w = do
    mwp <- windowProperty32 "_NET_WM_DESKTOP" w
    case mwp of
        Just x -> modifyClient w (\c -> c { desktops = Set.fromList x })
        _      -> return ()

decodeCChar :: [CChar] -> String
#ifdef UTF8
#undef UTF8
decodeCChar = UTF8.decode . map fromIntegral
#define UTF8
#else
decodeCChar = map (toEnum . fromIntegral)
#endif
