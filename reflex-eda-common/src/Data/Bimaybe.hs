{-#LANGUAGE DeriveDataTypeable #-}
module Data.Bimaybe where

import Data.Maybe
import Data.Semigroup
import Data.Bifunctor

import Data.Data

data Bimaybe a b
    = Neither
    | Lust a
    | Rust b
    | Both a b
    deriving(Eq, Ord, Read, Show, Data)

instance Bifunctor Bimaybe where
    bimap l r Neither = Neither
    bimap l r (Lust a) = Lust (l a)
    bimap l r (Rust b) = Rust (r b)
    bimap l r (Both a b) = Both (l a) (r b)

instance Functor (Bimaybe a) where
    fmap = second

instance (Semigroup a) => Applicative (Bimaybe a) where
    pure x = Rust x
    f <*> x = 
        let (l, r) = toMaybePair f
            (a, b) = toMaybePair x
        in fromMaybePair (l <> a, r <*> b)

instance (Semigroup a) => Monad (Bimaybe a) where
    return = pure
    Neither >>= k = Neither
    Lust a >>= k = Lust a
    Rust b >>= k = k b
    Both a b >>= k = case k b of
        Neither -> Neither
        Lust a' -> Lust (a <> a')
        Rust b' -> Both a b'
        Both a' b' -> Both (a <> a') b'

instance (Semigroup a, Semigroup b) => Monoid (Bimaybe a b) where
    mempty = Neither
    (toMaybePair -> (a, b)) `mappend` (toMaybePair -> (a', b')) =
        fromMaybePair (a <> a', b <> b')


bimaybe :: r -> (a -> r) -> (b -> r) -> (a -> b -> r) -> Bimaybe a b -> r
bimaybe x _ _ _ Neither = x
bimaybe _ f _ _ (Lust a) = f a
bimaybe _ _ f _ (Rust b) = f b
bimaybe _ _ _ f (Both a b) = f a b

fromBimaybe :: a -> b -> Bimaybe a b -> (a, b)
fromBimaybe a0 b0 (toMaybePair -> (a, b)) = (fromMaybe a0 a, fromMaybe b0 b)

extractLeft :: Bimaybe a b -> Maybe a
extractLeft = fst . toMaybePair

extractRight :: Bimaybe a b -> Maybe b
extractRight = snd . toMaybePair

{-# INLINABLE toMaybePair #-}
toMaybePair :: Bimaybe a b -> (Maybe a, Maybe b)
toMaybePair Neither = (Nothing, Nothing)
toMaybePair (Lust a) = (Just a, Nothing)
toMaybePair (Rust b) = (Nothing, Just b)
toMaybePair (Both a b) = (Just a, Just b)

{-# INLINABLE fromMaybePair #-}
fromMaybePair :: (Maybe a, Maybe b) -> Bimaybe a b
fromMaybePair (Nothing, Nothing) = Neither
fromMaybePair (Just a, Nothing) = Lust a
fromMaybePair (Nothing, Just b) = Rust b
fromMaybePair (Just a, Just b) = Both a b

{-#INLINABLE eitherToBimaybe #-}
eitherToBimaybe :: Either a b -> Bimaybe a b
eitherToBimaybe (Left a) = Lust a
eitherToBimaybe (Right b) = Rust b
