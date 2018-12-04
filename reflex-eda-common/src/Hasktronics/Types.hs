module Hasktronics.Types where

import Data.Text (Text)
import qualified Data.Text as T


type ComponentName = Text
type InstanceName = Text
type NetName = Text
type PinName = Text
type PinNo = Int -- one-indexed

type PinId = (ComponentName, PinNo)
