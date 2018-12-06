module Hasktronics.Netlist
    ( ComponentInfo(..)
    , ComponentLibrary
    , Netlist(..)
    , Part, Net
    , build
    ) where

import Hasktronics.Types
import Hasktronics.Expr
import Util

import Data.List
import Data.These
import Data.Bifunctor

import Text.Read (readEither)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map  as Map

import Control.Monad.State
import Control.Monad.Writer


data ComponentInfo = ComponentInfo
    { name :: ComponentName
    , pins :: [PinName]
    }
    deriving (Show)

type ComponentLibrary = Map ComponentName ComponentInfo

type Part = (PartName, ComponentInfo)
type Net = (NetName, [PinId])

data Netlist = Netlist
    { parts :: Map PartName Part
    , nets :: Map NetName Net
    }
    deriving (Show)


build :: Prog -> These [Text] (ComponentLibrary, Netlist) -- FIXME report warnings
build cmds = case runWriter (evalCmds cmds) of
    (val, []) -> That val
    (val, warns) -> These warns val

evalCmds :: [Cmd] -> Writer [Text] (ComponentLibrary, Netlist)
evalCmds cmds = do
    lib <- execStateT (evalLibCmd `traverse` cmds) lib0
    (_, parts) <- execStateT (evalComponentCmd `traverse` cmds) (lib, netlist0)
    (_, netlist) <- execStateT (evalNetCmd `traverse` cmds) (lib, parts)
    pure (lib, netlist)
    where
    lib0 = Map.empty
    netlist0 = Netlist Map.empty Map.empty

evalLibCmd :: Cmd -> StateT ComponentLibrary (Writer [Text]) ()
evalLibCmd (DefComponent{..}) = gets (Map.lookup name) >>= \case
    Nothing -> modify $ Map.insert name ComponentInfo{..}
    Just _ -> tell1 $ mconcat ["Component ", tshow name, " already exists: skipping."]
evalLibCmd _ = pure ()

evalComponentCmd :: Cmd -> StateT (ComponentLibrary, Netlist) (Writer [Text]) ()
evalComponentCmd (UseComponent partName compName) = do
    (lib, Netlist{..}) <- get
    case (Map.lookup compName lib, Map.lookup partName parts) of
        (Just comp, Nothing) -> modify $ second $ \nl ->
            nl{parts = Map.insert partName (partName, comp) parts}
        (Nothing, _) -> tell1 $ mconcat ["No definition for component: ", tshow compName, "."]
        (_, Just _) -> tell1 $ mconcat ["Component already defined: ", tshow partName, "."]
evalComponentCmd _ = pure ()

evalNetCmd :: Cmd -> StateT (ComponentLibrary, Netlist) (Writer [Text]) ()
evalNetCmd (Connect netName pins) = do
    Netlist{..} <- gets snd
    let (okPins, badPins) = partition (checkPinId parts) pins
    tell $ warnPin <$> badPins
    modify $ second $ \nl ->
        nl{nets = Map.alter (appendPins (netName, okPins)) netName nets}
    where
    checkPinId parts (partName, pinNo) = case Map.lookup partName parts of
        Just (_, ComponentInfo{..}) -> 1 <= pinNo && pinNo <= length pins
        Nothing -> False
    warnPin (compName, pinNo) = mconcat ["Undefined pin: `", compName, ".", tshow pinNo, "`"]
    appendPins :: Net -> Maybe Net -> Maybe Net
    appendPins (netName, newPins) Nothing = Just (netName, newPins)
    appendPins (netName, newPins) (Just (_, oldPins)) = Just (netName, oldPins ++ newPins)
evalNetCmd _ = pure ()
