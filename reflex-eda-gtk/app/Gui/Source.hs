{-# LANGUAGE QuasiQuotes #-} -- FIXME eliminate the initial value (which was just for debugging)
module Gui.Source where
import qualified NeatInterpolation as NI -- DEBUG

import Reflex.Dom
import Util.Dom

import Data.Text (Text)
import qualified Data.Text as T


header :: MonadWidget t m => m (Event t ())
header = do
    text "Compile "
    buttonWithStopPropagation "Go" ("class" =: "compile")

content :: MonadWidget t m => Event t () -> m (Event t Text)
content evGo = do
    ti <- textArea $ def
        { _textAreaConfig_attributes = constDyn $ mconcat
            [ "class" =: "compile"
            , "wrap" =: "off"
            , "rows" =: "60"
            , "cols" =: "80"
            ]
        , _textAreaConfig_initialValue = [NI.text|
[ LibCmd (DefComponent "hi" (DefPins
    [ DefPin "Vcc" []
    , DefPin "EN" []
    , DefPinsRange ("i", 1, 8) (DefPin "D" [i])
    ]))
, PartCmd (UseComponent "hi1" "hi")
, PartCmd (UseComponent "hi2" "hi")
, PartCmd (UseComponent "hi3" "hi")
, NetCmd (Connect "foo"
    [ PinByName "hi1" "Vcc" []
    , PinByName "hi2" "Vcc" []
    , PinByName "hi3" "Vcc" []
    ])
, NetCmd (ZipConnect "i" (1, 8) "d"
    [ PinByName "hi1" "D" [i]
    , PinByName "hi2" "D" [i]
    ])
]
|]
        }
    pure $ tagPromptlyDyn (_textArea_value ti) evGo