module Hasktronics.Expr where

import Hasktronics.Types
import Util

import Data.These
import Data.Bifunctor

import Text.Read (readEither)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Proxy
import GHC.TypeLits


type Prog = [Cmd]

data Cmd
    = DefComponent
        { name :: ComponentName
        , pins :: [PinName]
        }
    | UseComponent PartName ComponentName
    | Connect NetName [PinId]
    | DefGroup (Either Text Text) (Maybe Color)
    | Group Text (Either [NetName] [PartName])
    deriving (Read, Show)

parse :: Text -> These [Text] Prog
parse (T.unpack -> src) = either2these $ first ((:[]) . T.pack) $ readEither src



data Calc (env :: [(Symbol, *)]) a where
    Value :: a -> Calc env a
    Var :: (HasKey x env v) => Proxy x -> Calc env v
    Cat :: (Monoid t) => [Calc env t] -> Calc env t
    Range :: (Enum t) => Proxy x -> (t, t) -> Calc ('(x, t) ': env) a -> Calc env [a]

calculate :: Lower env -> Calc env t -> t
calculate env (Value x) = x
calculate env (Var x) = heteroLookup x env
calculate env (Cat xss) = mconcat $ calculate env <$> xss
calculate env (Range x (lo, hi) body) = [lo .. hi] <&> \i -> calculate ((x, i), env) body

type family Lower (m :: [(Symbol, *)]) = r | r -> m where
    Lower '[] = ()
    Lower ('(k, v) ': m) = ((Proxy k, v), Lower m)
class HasKey (k :: Symbol) (alist :: [(Symbol, *)]) v where
    heteroLookup :: Proxy k -> Lower alist -> v
instance HasKey k ('(k, v) ': rest) v where
    heteroLookup _ ((_, v), _) = v
instance (HasKey k m v) => HasKey k ('(k', v') : m) v where
    heteroLookup k (_, m) = heteroLookup k m
