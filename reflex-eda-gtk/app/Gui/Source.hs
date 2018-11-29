module Gui.Source where

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
        , _textAreaConfig_initialValue = "[DefComponent {name = \"hi\", pins = [\"a\", \"b\"]}, UseComponent \"bon soir\" \"hi\", Connect \"foo\" [(\"bon soir\", 1)], Connect \"foo\" [(\"hi\", 1), (\"bon soir\", 2)]]"
        }
    pure $ tagPromptlyDyn (_textArea_value ti) evGo