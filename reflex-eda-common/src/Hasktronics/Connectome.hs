module Hasktronics.Connectome where

import Hasktronics.Types
import Hasktronics.Expr
import Hasktronics.Netlist
import Util

import Data.Maybe
import Data.Either
import Data.These
import Data.Bifunctor
import Data.List

import Data.Text (Text)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State


type SegmentNo = Double
type NormalLoc = Double

type InstanceGroups = (MonoidalMap Text [Instance], [Instance])
type NetGroups = (MonoidalMap Text [NetName], [NetName])
data Connectome = Connectome
    { instanceGroups :: InstanceGroups
    , segmentCount :: Double
    , instanceLocs :: AList InstanceName (ComponentInfo, (SegmentNo, SegmentNo))
    , netPaths :: Map NetName [SegmentNo]
    , normal :: SegmentNo -> NormalLoc
    }


build :: Netlist -> Expr -> These [Text] Connectome
build netlist expr = case runWriter $ evalCmds netlist expr of
    (val, []) -> That val
    (val, warns) -> These warns val

evalCmds :: Netlist -> [Cmd] -> Writer [Text] Connectome
evalCmds netlist cmds = do
    let instanceGroups0 = (MMap.empty, Map.elems $ instances netlist)
    instanceGroups <- execStateT (runReaderT (evalConnectomeCmd `traverse` cmds) netlist) instanceGroups0
    -- FIXME down here, order component locations by group
    let segmentCount = calcSegmentCount netlist
        normal = (/ segmentCount)
        instanceLocs = calcInstanceLocs netlist instanceGroups
    netPaths <- calcNetPaths netlist instanceLocs
    pure Connectome{..}

evalConnectomeCmd :: Cmd -> ReaderT Netlist (StateT InstanceGroups (Writer [Text])) ()
-- evalConnectomeCmd (Group groupName (Left netNames)) = do -- TODO
evalConnectomeCmd (Group groupName (Right instNames)) = do
    instances <- asks instances
    let resolve instName = case Map.lookup instName instances of
            Nothing -> Left instName
            Just inst -> Right inst
    let (unknown, knownInstances) = partitionEithers $ resolve <$> instNames
    forM_ unknown $ \instName -> do
        tell1 $ mconcat ["Unknown instance ", instName, " cannot be grouped."]
    (groups, unassigned) <- get
    let (okInsts, alreadyAssigned) = partition check knownInstances
        check (fst -> instName) = instName `elem` (fst <$> unassigned)
    forM_ alreadyAssigned $ \(instName, _) -> do
        tell1 $ mconcat ["Component ", instName, " already assigned to a group."]
    let unassigned' = filter (\(name, _) -> name `notElem` (fst <$> okInsts)) unassigned
        groups' = groups <> MMap.singleton groupName okInsts
    put (groups', unassigned')
evalConnectomeCmd _ = pure ()


calcSegmentCount :: Netlist -> Double
calcSegmentCount Netlist{..} =
    let componentCount = Map.size instances
        pinCount = Map.foldl' (\acc (_, ComponentInfo{..}) -> acc + length pins) 0 instances
    in fromIntegral $ componentCount + pinCount

calcInstanceLocs :: Netlist -> InstanceGroups -> AList ComponentName (ComponentInfo, (SegmentNo, SegmentNo))
calcInstanceLocs Netlist{..} (assigned, unassigned) = runningTotal 0.0 insts
    where
    insts = (mconcat $ MMap.elems assigned) ++ unassigned
    runningTotal soFar [] = []
    runningTotal soFar ((instName, cInfo@ComponentInfo{..}):xs) =
        let start = soFar
            end = start + fromIntegral (length pins)
        in (instName, (cInfo, (start, end))) : runningTotal (end + 1.0) xs

calcNetPaths :: Netlist -> AList ComponentName (ComponentInfo, (SegmentNo, SegmentNo))
             -> Writer [Text] (Map NetName [Double])
calcNetPaths Netlist{..} instanceLocs = pure $ Map.map sort $ Map.mapWithKey f nets
    where
    f netName pinIds = do
        (component, fromIntegral -> pinNo) <- pinIds
        (_, (start, _)) <- maybeToList $ lookup component instanceLocs -- FIXME emit warning on Nothing
        pure $ start + (pinNo - 1.0)
