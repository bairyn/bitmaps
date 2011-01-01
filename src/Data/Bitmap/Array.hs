-- | Bitmaps as unboxed arrays of 32-bit RGBA pixels
--
-- These bitmaps are generally less efficient than 'BitmapString's, but can be arbitrarily
-- large (if having dimensions larger than 'Int' is really so useful), and have the advantage
-- of being managed in an array.

module Data.Bitmap.Array
    ( BitmapArray
    , bitmapArrayToArray
    , bitmapArrayToBitmapArray
    ) where

import Data.Array.Unboxed
import Data.Bitmap.Array.Internal
import Data.Word

bitmapArrayToArray :: BitmapArray -> UArray (Integer, Integer) Word32
bitmapArrayToArray = unwrapBitmapArray

bitmapArrayToBitmapArray :: UArray (Integer, Integer) Word32 -> BitmapArray
bitmapArrayToBitmapArray = BitmapArray
