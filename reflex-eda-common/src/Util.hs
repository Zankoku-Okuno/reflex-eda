module Util where

import Data.Text (Text)
import qualified Data.Text as T

import Data.These
import Control.Monad.Writer



tshow :: (Show a) => a -> Text
tshow = T.pack . show


(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)


type AList k v = [(k, v)]


either2these :: Either a b -> These a b
either2these (Left l) = This l
either2these (Right r) = That r

maybeThis :: These a b -> Maybe a
maybeThis (This a) = Just a
maybeThis (These a _) = Just a
maybeThis _ = Nothing

maybeThat :: These a b -> Maybe b
maybeThat (That a) = Just a
maybeThat (These _ a) = Just a
maybeThat _ = Nothing


tell1 :: MonadWriter [w] m => w -> m ()
tell1 x = tell [x]