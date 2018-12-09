module Hasktronics.Types where

import Hasktronics.Common
import qualified Data.Text as T


data Name = Name { baseName :: Text, indices :: [Word] }

instance Show Name where
    show Name{..} = T.unpack baseName ++ mkBrackets indices
        where
        mkBrackets [] = ""
        mkBrackets ixs = '[' : intercalate "," (show <$> ixs) `snoc` ']'

instance Read Name where
    readsPrec uhm input = do
        let (T.pack -> baseName, afterBase) = span isAlphaNum input
        guard (not $ T.null baseName)
        if "[" `isPrefixOf` afterBase
        then do
            (indices :: [Word], rest :: [Char]) <- readsPrec uhm afterBase
            guard (not $ null indices)
            pure $ (Name{..}, rest)
        else do
            let indices = []
            pure $ (Name{..}, afterBase)


type ComponentName = Text
type PartName = Text
type NetName = Text
type PinName = (Text, [Word])
type PinNo = Int -- one-indexed

type PinId = (PartName, PinNo)

type GroupName = Text
type Color = Text
