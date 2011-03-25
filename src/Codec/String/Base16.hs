{-# LANGUAGE ScopedTypeVariables #-}

module Codec.String.Base16
    ( encodeHex
    , decodeHex
    ) where

import Prelude hiding ((.), id, (++))
import Control.Applicative hiding (empty)
import Control.Category
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(mappend))
import Data.String.Class as S
import Data.Tagged
import Data.Word
import Numeric

encodeHex :: forall s. (StringCells s) => s -> s
encodeHex s
    | (Just (a_, s')) <- safeUncons s
        = case showHex (toWord8 a_) "" of
              (a:b:[]) -> (untag' . toMainChar $ a)   `cons` (untag' . toMainChar $ b) `cons` encodeHex s'
              (a:_)    -> (untag' . toMainChar $ '0') `cons` (untag' . toMainChar $ a) `cons` encodeHex s'
              _        -> encodeHex s'
    | otherwise
        = empty
    where untag' = untag :: Tagged s a -> a

decodeHex :: forall s. (StringCells s) => s -> Maybe s
decodeHex s
    | (Just (a, b, s')) <- safeUncons2 s
    , (Just w) <- (maybeRead $ "0x" ++ [toChar $ a] ++ [toChar $ b]  :: Maybe Word8)
        = ((untag' . toMainChar $ w) `cons`) <$> decodeHex s'
    | otherwise
        = Just empty
    where untag' = untag :: Tagged s a -> a

(++) :: (Monoid a) => a -> a -> a
(++) = mappend
infixr 5 ++

maybeRead :: (S.StringCells s) => Read a => s -> Maybe a
maybeRead = (>>= \ ~(r, s') -> if S.null s' then Just r else Nothing) . listToMaybe . reads . S.toStringCells
