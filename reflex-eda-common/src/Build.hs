module Build where

import Data.Default
import Data.Maybe
import Data.These
import Data.Bimaybe
import Data.Bifunctor
import Util


type BuildBuffer a b = Bimaybe a (Maybe a, b)

-- data BuildBuffer a b
--     = Zero
--     | Err a
--     | Working (Maybe a, b) a
--     | Good (Maybe a) b

isBuild :: BuildBuffer a b -> Bool
isBuild = isJust . extractRight

newestBuild :: BuildBuffer a b -> Bimaybe a b
newestBuild Neither = Neither
newestBuild (Lust err) = Lust err
newestBuild (Rust (Nothing, val)) = Rust val
newestBuild (Rust (Just warns, val)) = Both warns val
newestBuild (Both err _) = Lust err

prevGoodBuild :: BuildBuffer a b -> Maybe (Maybe a, b)
prevGoodBuild = extractRight

pushBuild :: These a b -> BuildBuffer a b -> BuildBuffer a b
pushBuild (This err) (toMaybePair -> (_, old)) = fromMaybePair (Just err, old)
pushBuild (That val) _ = Rust (Nothing, val)
pushBuild (These warn val) _ = Rust (Just warn, val)

-- instance Functor (BuildBuffer a) where
--     fmap f Zero = Zero
--     fmap f (Err err) = Err err
--     fmap f (Working (warn, stale) err) = Working (warn, (f stale)) err
--     fmap f (Good warn val) = Good warn (f val)
