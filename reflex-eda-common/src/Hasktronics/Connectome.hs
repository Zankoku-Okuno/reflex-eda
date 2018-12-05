module Hasktronics.Connectome where

import Hasktronics.Types
import Hasktronics.Expr as Expr
import Hasktronics.Netlist as Netlist
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

type PartGroups = ([GroupName], MonoidalMap GroupName [Part], [Part])
type NetGroups = ([GroupName], MonoidalMap GroupName [Net], [Net])

data Connectome = Connectome
    { partGroups :: PartGroups
    , partColors :: Map GroupName Color
    , netGroups :: NetGroups
    , netColors :: Map GroupName Color
    }

type PartLoc = (ComponentInfo, (SegmentNo, SegmentNo), Maybe GroupName)
type NetLoc = ([SegmentNo], Maybe GroupName)

build :: Netlist -> Expr -> These [Text] Connectome
build netlist expr = case runWriter $ evalCmds netlist expr of
    (val, []) -> That val
    (val, warns) -> These warns val

evalCmds :: Netlist -> [Cmd] -> Writer [Text] Connectome
evalCmds netlist cmds = do
    let groupNames0 = (([], Map.empty), ([], Map.empty))
    ((netGNames, netColors), (partGNames, partColors)) <-
        execStateT (evalDefCmd `traverse` cmds) groupNames0
    let partGroups0 = (partGNames, MMap.empty, Map.elems $ parts netlist)
        netGroups0 = (netGNames, MMap.empty, Map.elems $ nets netlist)
        groups0 = (netGroups0, partGroups0)
    (netGroups, partGroups) <- execStateT (runReaderT (evalAppendCmd `traverse` cmds) netlist) groups0
    -- FIXME down here, order component locations by group
    pure Connectome{..}

evalDefCmd :: Cmd -> StateT (([GroupName], Map GroupName Color), ([GroupName], Map GroupName Color)) (Writer [Text]) ()
evalDefCmd (DefGroup (Left name) color) = do
    (names, colors) <- gets fst
    if name `elem` names
    then tell1 $ mconcat ["Net group already defined: ", name]
    else do
        let names' = names ++ [name]
            colors' = maybe colors (\x -> Map.insert name x colors) color
        modify $ first $ const (names', colors')
evalDefCmd (DefGroup (Right name) color) = do
    (names, colors) <- gets snd
    if name `elem` names
    then tell1 $ mconcat ["Part group already defined: ", name]
    else do
        let names' = names ++ [name]
            colors' = maybe colors (\x -> Map.insert name x colors) color
        modify $ second $ const (names', colors')
evalDefCmd _ = pure ()

evalAppendCmd :: Cmd -> ReaderT Netlist (StateT (NetGroups, PartGroups) (Writer [Text])) ()
evalAppendCmd (Group groupName (Left netNames)) = do
    nets <- asks nets
    forM_ netNames $ \netName -> do
        (groupNames, groups, unassigned) <- gets fst
        case (Map.lookup netName nets, lookup netName unassigned, find (== groupName) groupNames) of
            (Nothing, _, _) -> tell1 $ mconcat ["Unknown net ", netName, " cannot be grouped."]
            (Just _, Nothing, _) -> tell1 $ mconcat ["Net ", netName, " already assigned to a group."]
            (Just _, Just _, Nothing) -> tell1 $ mconcat ["Unknown group: ", groupName]
            (Just net, Just _, Just _) -> do
                let unassigned' = filter ((/= netName) . fst) unassigned
                    groups' = groups <> MMap.singleton groupName [net]
                modify $ first $ const (groupNames, groups', unassigned')
evalAppendCmd (Group groupName (Right partNames)) = do
    parts <- asks parts
    forM_ partNames $ \partName -> do
        (groupNames, groups, unassigned) <- gets snd
        case (Map.lookup partName parts, lookup partName unassigned, find (== groupName) groupNames) of
            (Nothing, _, _) -> tell1 $ mconcat ["Unknown part ", partName, " cannot be grouped."]
            (Just _, Nothing, _) -> tell1 $ mconcat ["Component ", partName, " already assigned to a group."]
            (Just _, Just _, Nothing) -> tell1 $ mconcat ["Unknown group: ", groupName]
            (Just part, Just _, Just _) -> do
                let unassigned' = filter ((/= partName) . fst) unassigned
                    groups' = groups <> MMap.singleton groupName [part]
                modify $ second $ const (groupNames, groups', unassigned')
evalAppendCmd _ = pure ()


segmentCount :: Connectome -> Double
segmentCount Connectome{partGroups = (_, grouped, ungrouped)} =
    let componentCount = sum (MMap.map length grouped) + length ungrouped
        numsPins = (length . Netlist.pins . snd <$>)
        groupPinCounts = MMap.map (sum . numsPins) grouped
        nogroupPinCounts = numsPins ungrouped
        pinCount = sum groupPinCounts + sum nogroupPinCounts
    in fromIntegral $ componentCount + pinCount

normal :: Connectome -> (Double -> Double)
normal connectome = (/ segmentCount connectome)

partLocs :: Connectome -> Map PartName PartLoc
partLocs Connectome{..} = Map.fromList $ runningTotal 0.0 parts
    where
    (_, assigned, unassigned) = partGroups
    -- FIXME order groups according to list
    parts = (mconcat $ MMap.elems assigned) ++ unassigned
    findGroup partName = reverseLookup partName $ MMap.map (fst <$>) assigned
    runningTotal soFar [] = []
    runningTotal soFar ((partName, cInfo@ComponentInfo{..}):xs) =
        let start = soFar
            end = start + fromIntegral (length pins)
        in (partName, (cInfo, (start, end), findGroup partName)) : runningTotal (end + 1.0) xs

netPaths :: Connectome -> Map NetName NetLoc
netPaths c = Map.fromList $ f <$> nets
    where
    (_, assigned, unassigned) = netGroups c
    nets = (mconcat $ MMap.elems assigned) ++ unassigned :: [Net]
    findGroup netName = reverseLookup netName $ MMap.map (fst <$>) assigned
    f :: Net -> (NetName, NetLoc)
    f (netName, pinIds) = (netName, (sort points, findGroup netName))
        where
        points = do
            (component, fromIntegral -> pinNo) <- pinIds
            (_, (start, _), _) <- maybeToList $ Map.lookup component (partLocs c) -- NOTE compiling the netlist already issues a warning
            pure $ start + (pinNo - 1.0)


reverseLookup :: Eq a => a -> MonoidalMap k [a] -> Maybe k
reverseLookup x m = lookup x . concat $ mapSwap <$> MMap.toList m
    where
    mapSwap :: (k, [a]) -> [(a, k)]
    mapSwap (k, as) = (, k) <$> as
