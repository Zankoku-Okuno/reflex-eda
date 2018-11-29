module Hasktronics.Expr where

import Hasktronics.Types

type Expr = [Cmd]

data Cmd
    = DefComponent
        { name :: ComponentName
        , pins :: [PinName]
        }
    | UseComponent InstanceName ComponentName
    | Connect NetName [PinId]
    deriving (Read, Show)
