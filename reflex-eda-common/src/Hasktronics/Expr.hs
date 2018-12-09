module Hasktronics.Expr where

import Hasktronics.Expr.Base
import Hasktronics.Library
import Hasktronics.Netlist
import Hasktronics.Connectome

import Hasktronics.Common
import Hasktronics.Types

import Text.Read (readEither)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap


data Cmd
    = LibCmd LibraryCmd
    | PartCmd PartCmd
    | NetCmd NetCmd
    | DefGroupCmd DefGroupCmd
    | UseGroupCmd UseGroupCmd
    deriving (Read, Show)

parse :: Text -> These [Text] [Cmd]
parse (T.unpack -> src) = either2these $ first ((:[]) . T.pack) $ readEither src

buildLibrary :: [Cmd] -> These [Text] Library
buildLibrary = build $ \cmds -> do
    lib <- execStateT (goLibCmd `traverse` cmds) lib0
    pure $ Just lib
    where
    lib0 = Map.empty
    goLibCmd = \case { LibCmd cmd -> evalLibCmd cmd; _ -> pure () }

buildNetlist :: Library -> [Cmd] -> These [Text] Netlist
buildNetlist lib = build $ \cmds -> do
    parts <- execStateT (runReaderT (goPartCmd `traverse` cmds) lib) netlist0
    netlist <- execStateT (runReaderT (goNetCmd `traverse` cmds) (lib, Map.empty)) parts
    pure $ Just netlist
    where
    netlist0 = Netlist Map.empty Map.empty
    goPartCmd = \case { PartCmd cmd -> evalPartCmd cmd; _ -> pure () }
    goNetCmd = \case { NetCmd cmd -> evalNetCmd cmd; _ -> pure () }

buildConnectome :: Netlist -> [Cmd] -> These [Text] Connectome
buildConnectome netlist = build $ \cmds -> do
    ((netGNames, netColors), (partGNames, partColors)) <-
        execStateT (goDefGroupCmd `traverse` cmds) groupNames0
    let partGroups0 = (partGNames, MMap.empty, Map.elems $ parts netlist)
        netGroups0 = (netGNames, MMap.empty, Map.elems $ nets netlist)
        groups0 = (netGroups0, partGroups0)
    (netGroups, partGroups) <-
        execStateT (runReaderT (goUseGroupCmd `traverse` cmds) netlist) groups0
--     -- FIXME down here, order component locations by group
    pure $ Just Connectome{..}
    where
    groupNames0 = (([], Map.empty), ([], Map.empty))
    goDefGroupCmd = \case { DefGroupCmd cmd -> evalDefGroupCmd cmd; _ -> pure () }
    goUseGroupCmd = \case { UseGroupCmd cmd -> evalUseGroupCmd cmd; _ -> pure () }


build :: ([Cmd] -> Writer [Text] (Maybe a)) -> [Cmd] -> These [Text] a
build eval cmds = case runWriter $ eval cmds of
    (Nothing, errs) -> This errs
    (Just val, []) -> That val
    (Just val, warns) -> These warns val


-- evalCmds :: [Cmd] -> Writer [Text] (Library, Netlist)
-- evalCmds cmds = do
--     lib <- execStateT (goLibCmd `traverse` cmds) lib0
--     pure (lib, netlist)
--     where
