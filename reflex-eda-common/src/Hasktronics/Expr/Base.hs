module Hasktronics.Expr.Base where

import Hasktronics.Common
import Hasktronics.Types
import qualified Data.Text as T
import qualified Data.Map as Map


type VarName = Text


data NatExpr
    = Nat Word
    | NatVar VarName

type NatEnv = Map VarName Word

evalNatExpr :: NatExpr -> ReaderT NatEnv (Writer [Text]) (Maybe Word)
evalNatExpr (Nat n) = pure (Just n)
evalNatExpr (NatVar x) = do
    n <- asks (Map.lookup x)
    when (isNothing n) $ tell1 $ mconcat ["Undefined variable: ", tshow x]
    pure n

instance Show NatExpr where
    show (Nat n) = show n
    show (NatVar x) = T.unpack x

instance Read NatExpr where
    readsPrec uhm str =
        let lit = first Nat <$> readsPrec uhm str
            var = do
                guard (maybe False isAlpha $ safeHead str)
                let (T.pack -> x, rest) = span isAlphaNum str
                guard (not $ T.null x)
                pure $ (NatVar x, rest)
        in lit ++ var
