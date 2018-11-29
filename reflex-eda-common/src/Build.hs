module Build where

import Data.Default
import Data.These
import Data.Bifunctor
import Util


data BuildBuffer a b
    = Zero
    | Err a
    | Working (Maybe a, b) a
    | Good (Maybe a) b

isBuild :: BuildBuffer a b -> Bool
isBuild Zero = False
isBuild _ = True

newestBuild :: BuildBuffer a b -> Maybe (These a b)
newestBuild Zero = Nothing
newestBuild (Err err) = Just $ This err
newestBuild (Working _ err) = Just $ This err
newestBuild (Good (Just warn) new) = Just $ These warn new
newestBuild (Good Nothing new) = Just $ That new

prevGoodBuild :: BuildBuffer a b -> Maybe (These a b)
prevGoodBuild Zero = Nothing
prevGoodBuild (Err _) = Nothing
prevGoodBuild (Working (Just warn, old) _) = Just $ These warn old
prevGoodBuild (Working (Nothing, old) _) = Just $ That old
prevGoodBuild (Good _ _) = Nothing


pushBuild :: These a b -> BuildBuffer a b -> BuildBuffer a b
pushBuild (This err) old = case old of
    Zero -> Err err
    Err _ -> Err err
    Working old _ -> Working old err
    Good warn old -> Working (warn, old) err
pushBuild (That new) _ = Good Nothing new
pushBuild (These warn new) _ = Good (Just warn) new
