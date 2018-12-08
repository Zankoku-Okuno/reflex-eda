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
[ DefComponent "hi" (DefPins
    [ DefPin "EN" []
    , DefPinsRange ("i", 0, 7) (DefPin "D" [NatVar "i"])
    ])
, UseComponent "hi1" "hi"
]
|]
        }
    pure $ tagPromptlyDyn (_textArea_value ti) evGo