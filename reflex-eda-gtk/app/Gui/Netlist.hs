module Gui.Netlist where

import Build
import Hasktronics.Netlist
import Util

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg

import Data.Bimaybe
import Data.Monoid
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map  as Map


header :: MonadWidget t m => m ()
header = text "Netlist"

content :: MonadWidget t m => BuildBuffer [Text] Netlist -> m ()
content Neither = blank
content (Lust errs) = renderErrors errs
content (Rust (warns, val)) = do
    renderValue val
    maybe blank renderErrors warns
content (Both errs (warns, stale)) = do
    renderErrors errs
    el "hr" blank
    el "p" $ text "⚠️ stale"
    maybe blank renderErrors warns
    renderValue stale

renderErrors :: MonadWidget t m => [Text] -> m ()
renderErrors errs = el "p" $ do
    text $ tshow errs -- FIXME show a list

renderValue :: MonadWidget t m => Netlist -> m ()
renderValue val@Netlist{..} = do
    el "p" $ text $ tshow val
