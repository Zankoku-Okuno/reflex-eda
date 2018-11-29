module Main where

import Text.Read
import Data.Text (Text)
import qualified Data.Text as T


import Data.Default
import Data.Maybe
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
import qualified Gui.Source as Source
import Hasktronics.Netlist (Netlist)
import qualified Hasktronics.Netlist as Netlist
import qualified Gui.Netlist as Netlist

main :: IO ()
main = mainWidgetWithHead htmlhead $ do
    rec htmlTheme dynTheme
        dynTheme <- htmlNav
    htmlBody


htmlhead :: forall t m. (MonadWidget t m) => m ()
htmlhead = do
    el "style" $ text layout

htmlTheme :: MonadWidget t m => Dynamic t Text -> m ()
htmlTheme dynTheme = do
    el "style" $ dynText dynTheme

htmlNav :: forall t m. (MonadWidget t m) => m (Dynamic t Text)
htmlNav = el "nav" $ do
    el "h1" $ text "Reflex EDA"
    elClass "div" "spacer" $ blank
    evBtn <- button "Theme"
    dynState <- foldDyn (const tail) (cycle [theme_dark, theme_wrong]) evBtn
    pure $ head <$> dynState

htmlBody :: forall t m. (MonadWidget t m) => m ()
htmlBody = elAttr "div" ("id" =: "editor" ) $ do
    rec evFromTabs <- tabs 0 (1 <$ evResult)
            [ (0, constDyn True
                , constDyn $ Source.header
                , constDyn $ Source.content evGo
                )
            , (1, isBuild <$> dynResult
                , constDyn $ never <$ Netlist.header -- TODO indicate build status
                , (never <$) . Netlist.content <$> dynResult
                )
            ]
        let (evGo, evSrc) = fromJust $ Map.lookup 0 evFromTabs
            evResult = second snd . Netlist.build <$> evSrc
        dynResult <- foldDyn pushBuild Zero evResult
    blank
