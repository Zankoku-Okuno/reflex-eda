module Hasktronics.Library where

import Hasktronics.Common
import Hasktronics.Types
import Hasktronics.Expr.Base

import qualified Data.Map as Map


type Library = Map ComponentName ComponentInfo

data ComponentInfo = ComponentInfo
    { name :: ComponentName
    , pins :: [PinName]
    }
    deriving (Show)


data LibraryCmd
    = DefComponent ComponentName DefPins
    deriving (Read, Show)

evalLibCmd :: LibraryCmd -> StateT Library (Writer [Text]) ()
evalLibCmd (DefComponent name pinsExpr) = gets (Map.lookup name) >>= \case
    Just _ -> tell1 $ mconcat ["Component ", tshow name, " already exists: skipping."]
    Nothing -> do
        pins <- lift $ runReaderT (evalDefPins pinsExpr) Map.empty
        -- TODO check no duplicated names
        modify $ Map.insert name (ComponentInfo name pins)


data DefPins
    = DefPin Text [NatExpr]
    | DefPins [DefPins]
    | DefPinsRange (VarName, Word, Word) DefPins
    deriving(Read, Show)

evalDefPins :: DefPins -> ReaderT NatEnv (Writer [Text]) [PinName]
evalDefPins (DefPin base indexExprs) = do
    indices <- catMaybes <$> evalNatExpr `mapM` indexExprs
    pure [(base, indices)]
evalDefPins (DefPinsRange (x, lo, hi) expr) = do
    let inEnvs = [local (Map.insert x n) | n <- [lo..hi]]
    namess <- forM inEnvs ($ evalDefPins expr)
    pure $ concat namess
evalDefPins (DefPins exprs) = concat <$> evalDefPins `mapM` exprs
