module Gui.Netlist where

import Build
import Hasktronics.Netlist
import Util

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg

import Data.These
import Data.Monoid
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map  as Map


header :: MonadWidget t m => m ()
header = text "Netlist"

content :: MonadWidget t m => BuildBuffer [Text] Netlist -> m ()
content Zero = blank
content (Err errs) = renderErrors errs
content (Working (warns, stale) fresh) = do
    renderErrors fresh
    el "hr" blank
    el "p" $ text "⚠️ stale"
    maybe blank renderErrors warns
    renderValue stale
content (Good warns val) = do
    renderValue val
    maybe blank renderErrors warns

renderErrors :: MonadWidget t m => [Text] -> m ()
renderErrors errs = el "p" $ do
    text $ tshow errs -- FIXME show a list

renderValue :: MonadWidget t m => Netlist -> m ()
renderValue val@Netlist{..} = do
    svg "svg" $ do
        forM (zip [0..] $ Map.toList components) $ \(i, (instName, component)) -> do
            svgAttr "rect" (mconcat ["x" =: tshow (50*i), "y" =: "0", "width" =: "30", "height" =: "50"]) blank
        -- svgAttr "circle" (mconcat ["r" =: "25", "cx" =: "50", "cy" =: "50"]) $ blank
    el "p" $ text $ tshow val
