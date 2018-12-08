module Hasktronics.Expr where

import Hasktronics.Types
import Util

import Data.Maybe
import Data.These
import Data.Bifunctor

import Text.Read (readEither)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Monad.Writer


type VarName = Text


type Prog = [Cmd]

data Cmd
    = DefComponent ComponentName DefPins
    | UseComponent PartName ComponentName
    | Connect NetName [PinId]
    | DefGroup (Either Text Text) (Maybe Color)
    | Group Text (Either [NetName] [PartName])
    deriving (Read, Show)

parse :: Text -> These [Text] Prog
parse (T.unpack -> src) = either2these $ first ((:[]) . T.pack) $ readEither src

data DefPins
    = DefPin Text [NatExpr]
    | DefPins [DefPins]
    | DefPinsRange (VarName, Word, Word) DefPins -- TODO multiple ranges at once
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

data NatExpr
    = Nat Word
    | NatVar VarName
    deriving(Read, Show)
type NatEnv = Map VarName Word
evalNatExpr :: NatExpr -> ReaderT NatEnv (Writer [Text]) (Maybe Word)
evalNatExpr (Nat n) = pure (Just n)
evalNatExpr (NatVar x) = do
    n <- asks (Map.lookup x)
    when (isNothing n) $ tell1 $ mconcat ["Undefined variable: ", tshow x]
    pure n
