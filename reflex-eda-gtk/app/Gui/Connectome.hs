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
renderValue c = do
    (dynPartFilter, dynNetFilter) <- mkToggles c
    mkStyle c
    let dynConnectome = filterConnectome c <$> dynPartFilter <*> dynNetFilter
    void $ dyn $ mkSvg <$> dynConnectome
    where
    mkStyle :: MonadWidget t m => Connectome -> m ()
    mkStyle c@Connectome{..} = el "style" $ do
        sequence_ $ flip Map.mapWithKey partColors $ \groupName color -> do
            text $ mconcat
                [ ".connectome .components [data-group-name=", groupName, "]"
                , "{"
                , "stroke: ", color, ";"
                , "}\n"
                ]
        sequence_ $ flip Map.mapWithKey netColors $ \groupName color -> do
            text $ mconcat
                [ ".connectome .nets [data-group-name=", groupName, "]"
                , "{"
                , "stroke: ", color, ";"
                , "}\n"
                ]
    mkToggles :: MonadWidget t m => Connectome -> m (Dynamic t ([GroupName], Bool), Dynamic t ([GroupName], Bool))
    mkToggles c@Connectome{..} = do
        elClass "div" "group-toggles" $ do
            partConfig <- el "ol" $ do
                let (groupNames, _, _) = partGroups
                toggles <- forM groupNames $ \groupName -> el "li" $ do
                    toggle <- checkbox True def
                    text groupName
                    pure $ value toggle <&> (\on -> if on then [groupName] else [])
                noGToggle <- el "li" $ do
                    toggle <- checkbox True def
                    text "other"
                    pure $ value toggle
                pure $ zipDyn (concat <$> (sequence toggles)) noGToggle
            netConfig <- el "ol" $ do
                let (groupNames, _, _) = netGroups
                toggles <- forM groupNames $ \groupName -> el "li" $ do
                    toggle <- checkbox True def
                    text groupName
                    pure $ value toggle <&> (\on -> if on then [groupName] else [])
                noGToggle <- el "li" $ do
                    toggle <- checkbox True def
                    text "other"
                    pure $ value toggle
                pure $ zipDyn (concat <$> (sequence toggles)) noGToggle
            pure (partConfig, netConfig)
    mkSvg :: MonadWidget t m => Connectome -> m ()
    mkSvg c@Connectome{..} = do
        elAttr "div" ("class" =: "connectome aspect-9-16") $ do
            svgAttr "svg" ("height" =: tshow (2*(oR + margin))
                        <> "width"  =: tshow (2*(oR + margin))) $
                svgAttr "g" ("transform" =: svgTranslate margin margin) $ do
                    svgAttr "g" ("class" =: "nets" <> "transform" =: svgTranslate oR oR) $ do
                        sequence_ $ Map.mapWithKey netLayer (netPaths c)
                    svgClass "g" "components" $ do
                        sequence_ $ Map.mapWithKey componentLayer (partLocs c)
        where
        pinSeparation = 10.0
        oR = max 100.0 $ pinSeparation * segmentCount c / tau
        iR = oR - pinLength
        margin = 20.0
        pinLength = 17.0
        normalize = normal c
        componentLayer :: MonadWidget t m => PartName -> PartLoc -> m ()
        componentLayer partName (cInfo, (start, end), groupName) = svgAttr "g" (mconcat
                [ "transform" =: svgRotate (360.0 * normalize start) (Just (oR, oR))
                , "data-part-name" =: partName
                , "data-group-name" =: fromMaybe "" groupName
                ]) $ do
            let componentRadians = (tau*) . normalize $ end - start
                largeArc = normalize (end - start) > 0.5
            svgAttr "path" (mconcat
                [ "d" =: T.intercalate " "
                    [ svgPathM Abs oR 0.0
                    , svgPathA Rel (oR, oR) 0.0
                        largeArc True
                        (oR * sin componentRadians, oR * (versin componentRadians))
                    ]
                ]) blank
            svgClass "g" "pins" $ do
                forM_ (zip [0..] (pins cInfo)) $ \(fromIntegral -> pinNo, pinName) -> do
                    let pinDegrees = (360.0*) . normalize $ pinNo + 0.5
                    svgAttr "line" (mconcat
                        [ "x1" =: tshow oR, "x2" =: tshow oR
                        , "y1" =: "0" , "y2" =: tshow pinLength
                        , "transform" =: svgRotate pinDegrees (Just (oR, oR))
                        ]) $ blank
        netLayer :: MonadWidget t m => NetName -> NetLoc -> m ()
        netLayer netName (connects, groupName) = do
            let toCoords = negate . (tau*) . normalize . (+0.5)
                points = connects <&> \(toCoords -> loc) ->
                    let x = iR * negate (sin loc)
                        y = iR * negate (cos loc)
                    in tshow x <> "," <> tshow y
            svgAttr "polygon" (mconcat
                [ "points" =: T.intercalate " " points
                , "data-net-name" =: netName
                , "data-group-name" =: fromMaybe "" groupName
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