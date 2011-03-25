{-# LANGUAGE ScopedTypeVariables #-}

module Codec.String.Base64
    ( bytes64
    , ibytes64
    , fillByte64
    , encode64
    , decode64
    ) where

import Prelude hiding ((.), id, (++), length)
import Control.Applicative hiding (empty)
import Control.Category
import Data.Array.IArray
import Data.Bits
import qualified Data.Map as M
import Data.Monoid
import Data.String.Class
import Data.Tagged
import Data.Word

bytes64 :: Array Word8 Word8
bytes64 = listArray (0, 0x3F) $
    [0x41..0x5A]
 ++ [0x61..0x7A]
 ++ [0x30..0x39]
 ++ [0x2B, 0x2F]
    where (++) = mappend

ibytes64 :: M.Map Word8 Word8
ibytes64 = M.fromList $ map (\ ~(a_, b_) -> (b_, a_)) . assocs $ bytes64

fillByte64 :: Word8
fillByte64 = 0x3D

encode64 :: forall s. (StringCells s) => s -> s
encode64 s
    | (Just (a, b, c, s')) <- safeUncons3 s =
        let a'  = toWord8 a
            b'  = toWord8 b
            c'  = toWord8 c
            a'' = base $ a' `shiftR` 2
            b'' = base $ ((a' .&. 0x03) `shiftL` 4) .|. (b' `shiftR` 4)
            c'' = base $ ((b' .&. 0x0F) `shiftL` 2) .|. (c' `shiftR` 6)
            d'' = base $ c' .&. 0x3F
        in  cons4 a'' b'' c'' d'' $ encode64 s'
    | 2 <- length s                     =
        let ~(Just (a, b, _)) = safeUncons2 s
            a'  = toWord8 a
            b'  = toWord8 b
            a'' = base $ a' `shiftR` 2
            b'' = base $ ((a' .&. 0x03) `shiftL` 4) .|. (b' `shiftR` 4)
            c'' = base $ (b' .&. 0x0F) `shiftL` 2
        in cons4 a'' b'' c'' fillByte64' $ empty
    | 1 <- length s                     =
        let ~(Just (a, _)) = safeUncons s
            a'  = toWord8 a
            a'' = base $ a' `shiftR` 2
            b'' = base $ (a' .&. 0x03) `shiftL` 4
        in  cons4 a'' b'' fillByte64' fillByte64' $ empty
    | otherwise                         =
        empty
    where base = untag' . toMainChar . (bytes64 !) . toWord8
          fillByte64' = untag' . toMainChar $ fillByte64
          untag' = untag :: Tagged s a -> a

decode64 :: forall s. (StringCells s) => s -> Maybe s
decode64 s
    | (Just (a, b, c, d, s')) <- safeUncons4 s = do
          let n x
                  | (toWord8 x) == fillByte64                 = Just 0xFF  -- no regular base 64 digit can match with 0xFF; use this so we know whether a byte is a fill byte
                  | (Just y) <- M.lookup (toWord8 x) ibytes64 = Just y
                  | otherwise                                 = Nothing
          a' <- n a
          b' <- n b
          c' <- n c
          d' <- n d
          if c' /= 0xFF
              then do
                  -- c is not a fill byte; check d
                  if d' /= 0xFF
                      then do
                          -- abcd
                          cons3
                              (untag' . toMainChar $ (a' `shiftL` 2) .|. (b' `shiftR` 4))
                              (untag' . toMainChar $ (b' `shiftL` 4) .|. (c' `shiftR` 2))
                              (untag' . toMainChar $ (c' `shiftL` 6) .|. d')
                            <$> decode64 s'
                      else do
                          -- abc_
                          Just . cons2
                              (untag' . toMainChar $ (a' `shiftL` 2) .|. (b' `shiftR` 4))
                              (untag' . toMainChar $ (b' `shiftL` 4) .|. (c' `shiftR` 2))
                            $ empty
              else do
                  do
                      do
                          -- ab__
                          Just . cons
                              (untag' . toMainChar $ (a' `shiftL` 2) .|. (b' `shiftR` 4))
                            $ empty
    | otherwise =
        Just empty
    where untag' = untag :: Tagged s a -> a
