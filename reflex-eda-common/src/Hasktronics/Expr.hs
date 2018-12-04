module Hasktronics.Expr where

import Hasktronics.Types
import Util

import Data.These
import Data.Bifunctor

import Text.Read (readEither)
import Data.Text (Text)
import qualified Data.Text as T


type Expr = [Cmd]

data Cmd
    = DefComponent
        { name :: ComponentName
        , pins :: [PinName]
        }
    | UseComponent InstanceName ComponentName
    | Connect NetName [PinId]
    | Group Text (Either [NetName] [ComponentName])
    deriving (Read, Show)

parse :: Text -> These [Text] Expr
parse (T.unpack -> src) = either2these $ first ((:[]) . T.pack) $ readEither src
