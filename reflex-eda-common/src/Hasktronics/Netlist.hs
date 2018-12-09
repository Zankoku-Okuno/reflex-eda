module Hasktronics.Netlist where

import Hasktronics.Library

import Hasktronics.Common
import Hasktronics.Types
import Hasktronics.Expr.Base

import Text.Read (readEither)
import qualified Data.Text as T
import qualified Data.Map  as Map



type Part = (PartName, ComponentInfo)
type Net = (NetName, [PinId])

data Netlist = Netlist
    { parts :: Map PartName Part
    , nets :: Map NetName Net
    }
    deriving (Show)


data PartCmd
    = UseComponent PartName ComponentName
    deriving (Read, Show)

evalPartCmd :: PartCmd -> ReaderT Library (StateT Netlist (Writer [Text])) ()
evalPartCmd (UseComponent partName compName) = do
    Netlist{..} <- get
    comp <- asks $ Map.lookup compName
    case (comp, Map.lookup partName parts) of
        (Just comp, Nothing) -> modify $ \nl ->
            nl{parts = Map.insert partName (partName, comp) parts}
        (Nothing, _) -> tell1 $ mconcat ["No definition for component: ", tshow compName, "."]
        (_, Just _) -> tell1 $ mconcat ["Component already defined: ", tshow partName, "."]


data NetCmd
    = Connect NetName [PinExpr]
    | ZipConnect VarName (Word, Word) NetName [PinExpr] -- FIXME make the netname a NameExpr
    deriving (Read, Show)

evalNetCmd :: NetCmd -> ReaderT (Library, NatEnv) (StateT Netlist (Writer [Text])) ()
evalNetCmd (Connect netName pins) = do
    (lib, natEnv) <- ask
    parts <- gets parts
    (catMaybes -> okPins) <- lift . lift $
        runReaderT (evalPinExpr `traverse` pins) (lib, parts, natEnv)
    Netlist{..} <- get
    modify $ \nl ->
        nl{nets = Map.alter (appendPins (netName, okPins)) netName nets}
    where
    appendPins :: Net -> Maybe Net -> Maybe Net
    appendPins (netName, newPins) Nothing = Just (netName, newPins)
    appendPins (netName, newPins) (Just (_, oldPins)) = Just (netName, oldPins ++ newPins)
evalNetCmd (ZipConnect x (lo, hi) baseName pinExprs) = do
    forM_ [lo..hi] $ \n -> local (second $ Map.insert x n) $ do
        evalNetCmd (Connect (baseName <> "." <> tshow n) pinExprs)


data PinExpr
    = PinByNumber PartName Int
    | PinByName PartName Text [NatExpr]
    deriving(Read, Show)

evalPinExpr :: PinExpr -> ReaderT (Library, Map PartName Part, NatEnv) (Writer [Text]) (Maybe PinId) -- FIXME I need a component library, too
evalPinExpr (PinByNumber c n) = pure $ Just (c, n)
    -- FIXME check that the requested pin exists
    -- let (okPins, badPins) = partition (checkPinId parts) pins
    -- tell $ warnPin <$> badPins
    -- where
    -- checkPinId parts (partName, pinNo) = case Map.lookup partName parts of
    --     Just (_, ComponentInfo{..}) -> 1 <= pinNo && pinNo <= length pins
    --     Nothing -> False
    -- warnPin (compName, pinNo) = mconcat ["Undefined pin: `", compName, ".", tshow pinNo, "`"]
evalPinExpr (PinByName partName name ixs) = do
    (libEnv, parts, natEnv) <- ask
    (catMaybes -> ixs) <- lift $ runReaderT (evalNatExpr `traverse` ixs) natEnv
    case Map.lookup partName parts of
        Nothing -> do
            tell1 (mconcat ["Can't find part: ", partName])
            pure Nothing
        Just (_, cInfo) -> case elemIndex (name, ixs) (pins cInfo) of
            Nothing -> do
                tell1 (mconcat
                    [ "Part ", partName
                    , " has no pin "
                    , name, "[", T.intercalate "," (tshow <$> ixs), "]"
                    ])
                pure Nothing
            Just ix -> pure $ Just (partName, ix + 1)
