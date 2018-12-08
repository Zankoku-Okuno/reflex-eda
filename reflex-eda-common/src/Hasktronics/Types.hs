module Hasktronics.Types where

import Data.Text (Text)
import qualified Data.Text as T


type ComponentName = Text
type PartName = Text
type NetName = Text
type PinName = (Text, [Word])
type PinNo = Int -- one-indexed

type PinId = (ComponentName, PinNo)

type GroupName = Text
type Color = Text
