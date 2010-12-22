{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

module Codec.String.Base64
    ( bytes64
    , fillByte64
    , encode64
    , decode64
    ) where

import Prelude hiding ((.), id, (++), length)
import Control.Category
import Data.Array.IArray
import Data.Bits
import Data.Monoid
import Data.String.Class
import Data.Word

bytes64 :: Array Word8 Word8
bytes64 = listArray (0, 0x3F) $
    [0x41..0x5A]
 ++ [0x61..0x7A]
 ++ [0x30..0x39]
 ++ [0x2B, 0x2F]
    where (++) = mappend

fillByte64 :: Word8
fillByte64 = 0x3D

encode64 :: forall s. (StringConstruct s, StringLength s, StringEmpty s) => s -> s
encode64 s
    | (Just (a, b, c, s')) <- uncons3 s =
        let a'  = toWord8 a
            b'  = toWord8 b
            c'  = toWord8 c
            a'' = base $ a' `shiftR` 2
            b'' = base $ ((a' .&. 0x03) `shiftL` 4) .|. (b' `shiftR` 4)
            c'' = base $ ((b' .&. 0x0F) `shiftL` 2) .|. (c' `shiftR` 6)
            d'' = base $ c' .&. 0x3F
        in  cons4 a'' b'' c'' d'' $ encode64 s'
    | 2 <- length s  :: Integer         =
        let ~(Just (a, b, _)) = uncons2 s
            a'  = toWord8 a
            b'  = toWord8 b
            a'' = base $ a' `shiftR` 2
            b'' = base $ ((a' .&. 0x03) `shiftL` 4) .|. (b' `shiftR` 4)
            c'' = base $ (b' .&. 0x0F) `shiftL` 2
        in cons4 a'' b'' c'' fillByte64' $ empty
    | 1 <- length s  :: Integer         =
        let ~(Just (a, _)) = uncons s
            a'  = toWord8 a
            a'' = base $ a' `shiftR` 2
            b'' = base $ (a' .&. 0x03) `shiftL` 4
        in  cons4 a'' b'' fillByte64' fillByte64' $ empty
    | otherwise                         =
        empty
    where base = toMainChar key . (bytes64 !) . toWord8
          fillByte64' = toMainChar key fillByte64
          key = keyStringConstruct :: s

decode64 :: forall s. (StringConstruct s, StringEmpty s) => s -> s
decode64 s
    | (Just (a, b, c, d, s')) <- uncons4 s =
          let n x
                  | (toWord8 x) == fillByte64 = 0xFF
                  | (Just y) <- lookup (toWord8 x) . map (\ ~(a_, b_) -> (b_, a_)) . assocs $ bytes64 = y
                  | otherwise = 0x00
              a' = n a
              b' = n b
              c' = n c
              d' = n d
          in  case () of
                     _ | c' /= 0xFF ->
                             case () of
                                    _ | d' == 0xFF ->
                                          cons2
                                              (toMainChar key $ (a' `shiftL` 2) .|. (b' `shiftR` 4))
                                              (toMainChar key $ (b' `shiftL` 4) .|. (c' `shiftR` 2))
                                            $ empty
                                      | otherwise  ->
                                          cons3
                                              (toMainChar key $ (a' `shiftL` 2) .|. (b' `shiftR` 4))
                                              (toMainChar key $ (b' `shiftL` 4) .|. (c' `shiftR` 2))
                                              (toMainChar key $ (c' `shiftL` 6) .|. d')
                                            $ decode64 s'
                       | otherwise ->
                           cons
                               (toMainChar key $ (a' `shiftL` 2) .|. (b' `shiftR` 4))
                             $ empty
    | otherwise =
        empty
    where key = keyStringConstruct :: s
