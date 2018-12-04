module Gui.Connectome where

import Control.Monad.IO.Class -- DEBUG

import Hasktronics.Types
import Hasktronics.Netlist
import Hasktronics.Connectome
import Build
import Util

import Data.Maybe
import Data.Bimaybe
import Data.List
import Data.Bifunctor
import Data.Monoid
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Css

header :: MonadWidget t m => m ()
header = text "Connectome"

content :: MonadWidget t m => BuildBuffer [Text] Connectome -> m ()
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

renderValue :: forall t m. MonadWidget t m => Connectome -> m ()
renderValue val@Connectome{..} = do
    elAttr "div" ("class" =: "connectome aspect-9-16") $ do
        svgAttr "svg" ("height" =: tshow (2*(oR + margin))
                    <> "width"  =: tshow (2*(oR + margin))) $ 
            svgAttr "g" ("transform" =: svgTranslate margin margin) $ do
                svgAttr "g" ("class" =: "nets" <> "transform" =: svgTranslate oR oR) $ do
                    forM_ (Map.toList netPaths) $ netLayer
                svgClass "g" "components" $ do
                    forM_ instanceLocs componentLayer
    where
    pinSeparation = 10.0
    oR = max 100.0 $ pinSeparation * segmentCount / tau
    iR = oR - pinLength
    margin = 20.0
    pinLength = 17.0
    componentLayer :: MonadWidget t m => (InstanceName, (ComponentInfo, (SegmentNo, SegmentNo))) -> m ()
    componentLayer (instName, (cInfo, (start, end))) = svgAttr "g" (mconcat
            [ "transform" =: svgRotate (360.0 * normal start) (Just (oR, oR))
            , "data-instance-name" =: instName
            ]) $ do
        let componentRadians = (tau*) . normal $ end - start
        svgAttr "path" (mconcat
            [ "d" =: T.intercalate " "
                [ svgPathM Abs oR 0.0
                , svgPathA Rel (oR, oR) 0.0 False True
                    (oR * sin componentRadians, oR * (versin componentRadians))
                ]
            ]) blank
        svgClass "g" "pins" $ do
            forM_ (zip [0..] (pins cInfo)) $ \(fromIntegral -> pinNo, pinName) -> do
                let pinDegrees = (360.0*) . normal $ pinNo + 0.5
                svgAttr "line" (mconcat
                    [ "x1" =: tshow oR, "x2" =: tshow oR
                    , "y1" =: "0" , "y2" =: tshow pinLength
                    , "transform" =: svgRotate pinDegrees (Just (oR, oR))
                    ]) $ blank
    netLayer :: MonadWidget t m => (NetName, [SegmentNo]) -> m ()
    netLayer (netName, connects) = do
        let toCoords = negate . (tau*) . normal . (+0.5)
            points = connects <&> \(toCoords -> loc) ->
                let x = iR * negate (sin loc)
                    y = iR * negate (cos loc)
                in tshow x <> "," <> tshow y
        svgAttr "polygon" (mconcat 
            [ "points" =: T.intercalate " " points
            , "data-net-name" =: netName
            ]) blank


svgTranslate :: Double -> Double -> Text
svgTranslate x y = mconcat ["translate(", tshow x, ",", tshow y,")"]

svgRotate :: Double -> Maybe (Double, Double) -> Text
svgRotate degrees center = mconcat $ ["rotate(", tshow degrees] ++ around center ++ [")"]
    where
    around Nothing  = []
    around (Just (cx, cy)) = [",", tshow cx, ",", tshow cy]

data AR = Abs | Rel deriving (Read, Show, Eq, Ord, Enum)

svgPathM :: AR -> Double -> Double -> Text
svgPathM ar x y = mconcat [case ar of {Abs -> "M"; Rel -> "m"}, tshow x, ",", tshow y]

svgPathA :: AR -> (Double, Double) -> Double -> Bool -> Bool -> (Double, Double) -> Text
svgPathA ar (rx, ry) degrees largeArc sweep (x, y) = mconcat
    [ case ar of {Abs -> "A"; Rel -> "a"}
    , tshow rx, ","
    , tshow ry, ","
    , tshow degrees, ","
    , if largeArc then "1" else "0", ","
    , if sweep then "1" else "0", ","
    , tshow x, ","
    , tshow y
    ]

tau :: Fractional a => a
tau = 6.28318
versin theta = 1 - cos theta
vercos theta = 1 - sin theta