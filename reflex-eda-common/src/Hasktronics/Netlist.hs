module Hasktronics.Netlist where

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

data Netlist = Netlist
    { components :: Map InstanceName ComponentInfo
    , nets :: Map NetName [PinId]
    }
    deriving (Show)



eval :: Expr -> These [Text] (ComponentLibrary, Netlist) -- FIXME report warnings
eval cmds = case runWriter (evalCmds cmds) of
    (val, []) -> That val
    (val, warns) -> These warns val

evalCmds :: [Cmd] -> Writer [Text] (ComponentLibrary, Netlist)
evalCmds cmds = do
    lib <- execStateT (evalLibCmd `traverse` cmds) lib0
    (_, components) <- execStateT (evalComponentCmd `traverse` cmds) (lib, netlist0)
    (_, netlist) <- execStateT (evalNetCmd `traverse` cmds) (lib, components)
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
evalComponentCmd (UseComponent instName compName) = do
    (lib, Netlist{..}) <- get
    case (Map.lookup compName lib, Map.lookup instName components) of
        (Just comp, Nothing) -> modify $ second $ \nl ->
            nl{components = Map.insert instName comp components}
        (Nothing, _) -> tell1 $ mconcat ["No definition for component: ", tshow compName, "."]
        (_, Just _) -> tell1 $ mconcat ["Component already defined: ", tshow instName, "."]
evalComponentCmd _ = pure ()

evalNetCmd :: Cmd -> StateT (ComponentLibrary, Netlist) (Writer [Text]) ()
evalNetCmd (Connect netName pins) = do
    Netlist{..} <- gets snd
    let (okPins, badPins) = partition (checkPinId components) pins
    tell $ warnPin <$> badPins
    modify $ second $ \nl ->
        nl{nets = Map.alter (Just . maybe okPins (++ okPins)) netName nets}
    where
    checkPinId components (instName, pinNo) = case Map.lookup instName components of
        Just (ComponentInfo{..}) -> 1 <= pinNo && pinNo <= length pins
        Nothing -> False
    warnPin (compName, pinNo) = mconcat ["Undefined pin: `", compName, ".", tshow pinNo, "`"]
evalNetCmd _ = pure ()

build :: Text -> These [Text] (ComponentLibrary, Netlist)
build (T.unpack -> src) = case readEither src of
    Left err -> This [T.pack err]
    Right expr -> eval expr
