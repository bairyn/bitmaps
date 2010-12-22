{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- | Module containing semi-public internals of 'Data.ABitmap'
--
-- This exposes the internal and low level construction functions.  The API is
-- not stable.
--
-- Whenever possible, applications should instead use the functions from the
-- normal public interface modules.  Packages that extend the bitmap system at
-- a low level will need to use this module.
module Data.ABitmap.Internal
    ( PixelBGRA
    , PixelRGB
    , Bitmap(..)
    , BitmapRGB(..)
    , ForeignBitmap(..)
    ) where

import Data.Array.IArray
import qualified Data.Bitmap.Base as FB
import Data.Data
import Data.Serialize
import Data.Word

-- | A 32-bit pixel with a true color depth of 24 bits for the R, G and B components
--
-- A pixel of this type is represented in memory as B, G, R, A, from most-significant byte to least-significant byte.
type PixelBGRA = Word32

-- | A 24-bit RGB pixel without alpha information in 4 bytes in which the most significant byte is unused
type PixelRGB = Word32

-- | A bitmap of 'PixelBGRA' with a color depth of 32 bits; there are 8 bits for the four components
--
-- Unlike ForeignBitmap, this container is thread safe and pure.
-- It is represented as an array from (0, 0) to (numRows - 1, numColumns - 1).  These Bitmaps
-- are row-major.  The first pixel is top-left-most, and the second is the one immediately to
-- the right of that pixel unless the bitmap has only one column.
newtype Bitmap = Bitmap {unwrapBitmap :: Array (Integer, Integer) PixelBGRA}
    deriving (Eq, Ord, Data, Typeable, Serialize)

-- | A bitmap of 'PixelRGB' with a color depth of 24 bits
--
-- A 'BitmapRGB' is similar to a 'Bitmap' except that it lacks the alpha component, and
-- that its pixels are represented in the order "red, green, blue".
-- Values of this type are usually intermediary and used when loading from specific formats
-- before being converted to the standard 'Bitmap' type.
newtype BitmapRGB = BitmapRGB {unwrapBitmapRGB :: Array (Integer, Integer) PixelRGB}
    deriving (Eq, Ord, Data, Typeable, Serialize)

instance Show Bitmap where
    --show = map (chr . fromIntegral) . elems . unwrapBitmap
    show = show . unwrapBitmap

instance Show BitmapRGB where
    --show = map (chr . fromIntegral) . elems . unwrapBitmapRGB
    show = show . unwrapBitmapRGB

-- | A reference type to a bitmap
--
-- The bitmap is also represented in memory as BGR(|A).
newtype ForeignBitmap = ForeignBitmap {unwrapForeignBitmap :: FB.Bitmap Word8}
    deriving (Show)
