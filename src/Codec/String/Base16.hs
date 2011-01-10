{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

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
import Data.Word
import Numeric

encodeHex :: forall s. (StringCells s) => s -> s
encodeHex s
    | (Just (a_, s')) <- safeUncons s
        = case showHex (toWord8 a_) "" of
              (a:b:[]) -> (toMainChar key $ a)   `cons` (toMainChar key $ b) `cons` encodeHex s'
              (a:_)    -> (toMainChar key $ '0') `cons` (toMainChar key $ a) `cons` encodeHex s'
              _        -> encodeHex s'
    | otherwise
        = empty
    where key = keyStringCells :: s

decodeHex :: forall s. (StringCells s) => s -> Maybe s
decodeHex s
    | (Just (a, b, s')) <- safeUncons2 s
    , (Just w) <- (maybeRead $ "0x" ++ [toChar $ a] ++ [toChar $ b]  :: Maybe Word8)
        = ((toMainChar key w) `cons`) <$> decodeHex s'
    | otherwise
        = Just empty
    where key = keyStringCells :: s

(++) :: (Monoid a) => a -> a -> a
(++) = mappend
infixr 5 ++

maybeRead :: (S.StringCells s) => Read a => s -> Maybe a
maybeRead = (>>= \ ~(r, s') -> if S.null s' then Just r else Nothing) . listToMaybe . reads . S.toStringCells
