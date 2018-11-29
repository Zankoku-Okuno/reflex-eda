module Util.Dom where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map  as Map

import Data.Maybe
import Control.Monad

import Data.Proxy

import Reflex.Dom
import Control.Lens ((%~))
import Util


-- | based on https://gist.github.com/3noch/134b1ee7fa48c347be9d164c3fac4ef7
buttonWithStopPropagation :: forall t m. (DomBuilder t m) => Text -> Map AttributeName Text -> m (Event t ())
buttonWithStopPropagation btnText attrs = do
    let addClickFlag flag = addEventSpecFlags (Proxy @(DomBuilderSpace m)) Click (const flag)
    let cfg = def @(ElementConfig EventResult t (DomBuilderSpace m))
                & elementConfig_eventSpec %~ addClickFlag stopPropagation
                & elementConfig_initialAttributes .~ attrs
    (e, result) <- element "button" cfg $ text btnText
    pure $ domEvent Click e


tabs :: forall t m k content header. (MonadWidget t m, Ord k, Show k)
    => k
    -> Event t k
    -> [(k, Dynamic t Bool, Dynamic t (m (Event t header)), Dynamic t (m (Event t content)))]
    -> m (Map k (Event t header, Event t content))
tabs initActiveTab setActiveTab tabs = do
    rec (fromHeaders, evActivate) <- elClass "ul" "tabs" $ do
            stuff <- forM tabs $ \(tabId, dynVisible, dynAction, _) -> do
                let dynActive = (tabId ==) <$> dynActiveTab
                (liElem, fromHeader) <- elDynAttr' "li" (liAttrs <$> dynVisible <*> dynActive) $ do
                    evEvHeader <- dyn dynAction
                    evHeader<- switchHold never evEvHeader
                    pure evHeader
                let evClick = domEvent Click liElem
                    dynActivate = constDyn tabId
                pure (tagPromptlyDyn dynActivate evClick, (tabId, fromHeader))
            let activateEvents = fst <$> stuff
                fromHeaders = snd <$> stuff
            pure (fromHeaders, leftmost (setActiveTab : activateEvents))
        dynActiveTab <- holdDyn initActiveTab evActivate
    evs <- elClass "div" "tab" $
        forM tabs $ \(tabId, dynVisible, _, dynAction) -> do
            elDynAttr "div" (divAttrs tabId <$> dynVisible <*> dynActiveTab) $ do
                evEvContent <- dyn dynAction
                fromContent <- switchHold never evEvContent
                let fromHeader = fromJust $ lookup tabId fromHeaders
                pure $ tabId =: (fromHeader, fromContent)
    pure $ mconcat evs
    where
    liAttrs visible active = mconcat
        [ if visible then mempty else "style" =: "display:none;"
        , if active then "class" =: "active" else mempty
        ]
    divAttrs tabId visible activeId = mconcat
        [ if visible && tabId == activeId then mempty else "style" =: "display:none;"
        ]