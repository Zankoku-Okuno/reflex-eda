module Main where

import Text.Read
import Data.Text (Text)
import qualified Data.Text as T


import Data.Default
import Data.Maybe
import Data.Bimaybe
import Data.These
import Data.Monoid
import Data.Bifunctor
import Control.Applicative
import Control.Monad
import Util

import Data.Map (Map)
import qualified Data.Map  as Map
import Control.Monad.IO.Class

import Reflex.Dom
import Util.Dom

import Css

import Build
import Hasktronics
import Hasktronics.Expr (Cmd)
import qualified Hasktronics.Expr as Expr
import qualified Gui.Source as Source
import Hasktronics.Library (Library)
import qualified Hasktronics.Library as Library
import Hasktronics.Netlist (Netlist)
import qualified Hasktronics.Netlist as Netlist
import qualified Gui.Netlist as Netlist
import Hasktronics.Connectome (Connectome)
import qualified Hasktronics.Connectome as Connectome
import qualified Gui.Connectome as Connectome

main :: IO ()
main = mainWidgetWithHead htmlhead $ do
    rec htmlTheme dynTheme
        dynTheme <- htmlNav
    htmlBody


htmlhead :: forall t m. (MonadWidget t m) => m ()
htmlhead = do
    el "style" $ text layout

htmlTheme :: MonadWidget t m => Dynamic t Theme -> m ()
htmlTheme dynTheme = do
    el "style" $ dynText (theme <$> dynTheme)
    el "style" $ dynText (connectome <$> dynTheme)

htmlNav :: forall t m. (MonadWidget t m) => m (Dynamic t Theme)
htmlNav = el "nav" $ do
    el "h1" $ text "Reflex EDA"
    elClass "div" "spacer" $ blank
    evBtn <- button "Theme"
    dynState <- foldDyn (const tail) (cycle [theme_dark, theme_wrong]) evBtn
    pure $ head <$> dynState

htmlBody :: forall t m. (MonadWidget t m) => m ()
htmlBody = elAttr "div" ("id" =: "editor" ) $ do
    rec evFromTabs <- tabs 0 (2 <$ evNetlist)
            [ (0, constDyn True
                , constDyn $ Source.header
                , constDyn $ Source.content evGo
                )
            , (1, isBuild <$> dynNetlist
                , constDyn $ never <$ Netlist.header -- TODO indicate build status
                , (never <$) . Netlist.content <$> dynNetlist
                )
            , (2, isBuild <$> dynConnectome
                , constDyn $ never <$ Connectome.header -- TODO indicate build status
                , (never <$) . Connectome.content <$> dynConnectome
                )
            , (100, not . null <$> dynErrors
                , constDyn $ never <$ text "Errors"
                , (never <$) . text . tshow <$> dynErrors
                )
            ]
        let (evGo, evSrc) = fromJust $ Map.lookup 0 evFromTabs
        let evBuild :: Event t (These [Text] (Library, Netlist, Connectome))
            evBuild = evSrc <&> \src -> do
                program <- Expr.parse src
                lib <- Expr.buildLibrary program
                netlist <- Expr.buildNetlist lib program
                connectome <- Expr.buildConnectome netlist program
                -- (lib, netlist) <- Netlist.build expr
                -- (connectome :: Connectome) <- Connectome.build netlist expr
                pure (lib, netlist, connectome)
            evLib = second (\(x, _, _) -> x) <$> evBuild
            evNetlist = second (\(_, x, _) -> x) <$> evBuild
            evConnectome = second (\(_, _, x) -> x) <$> evBuild
            evErrors = fromMaybe [] . maybeThis <$> evBuild
        dynNetlist <- foldDyn pushBuild Neither evNetlist
        dynConnectome <- foldDyn pushBuild Neither evConnectome
        dynErrors <- holdDyn [] evErrors
    blank
