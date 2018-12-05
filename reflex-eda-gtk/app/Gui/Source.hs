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
[ DefComponent {name = "hi", pins = ["a", "b"]}
, DefComponent {name = "yo", pins = ["a", "b", "c", "d"]}
, UseComponent "bon soir 1" "hi"
, UseComponent "bon soir 2" "hi"
, UseComponent "bon soir 3" "hi"
, UseComponent "U1" "yo"
, UseComponent "U2" "yo"
, UseComponent "U3" "yo"
, Connect "pwr" [("bon soir 1", 1), ("bon soir 2", 1), ("bon soir 3", 1)]
, Connect "daisy" [("bon soir 1", 2), ("bon soir 2", 2)]
, Connect "pwr" [("bon soir 3", 2)]
, DefGroup (Right "U") (Just "#3B1")
, Group "U" (Right ["bon soir 1", "U1", "U2", "U3"])
, DefGroup (Left "Vcc") (Just "#D31")
, Group "Vcc" (Left ["pwr"])
]
|]
        }
    pure $ tagPromptlyDyn (_textArea_value ti) evGo