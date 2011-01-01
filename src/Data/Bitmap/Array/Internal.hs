{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}

module Data.Bitmap.Array.Internal
    ( BitmapArray(..)
    ) where

import Data.Array.Unboxed
import Data.Bitmap.Class
import Data.Bitmap.Pixel
import Data.Bitmap.Reflectable
import Data.Bitmap.Searchable
import Data.Bitmap.Types
import Data.Serialize
import Data.Word

-- | Arrays of 32-bit RGBA pixels
newtype BitmapArray = BitmapArray {unwrapBitmapArray :: UArray (Integer, Integer) Word32}
    deriving (Eq, Ord, Serialize)

-- | Instance for debugging purposes
instance Show BitmapArray where
    --show = map (chr . fromIntegral) . elems . unwrapBitmap
    show = show . unwrapBitmapArray

instance Bitmap BitmapArray where
    type BIndexType BitmapArray = Integer

    depth = const Depth32RGBA

    dimensions (BitmapArray a) =
        let (_, (maxRow, maxColumn)) = bounds a
        in  (abs . succ $ maxColumn, abs . succ $ maxRow)

    getPixel (BitmapArray a) = PixelRGBA . (a !)

    constructPixels f (width, height) = let maxRow    = abs . pred $ height
                                            maxColumn = abs . pred $ width
                                        in  BitmapArray . array ((0, 0), (maxRow, maxColumn)) $ [(i, unwrapPixelStorage . toPixelRGBA . f $ i) | row <- [0..maxRow], column <- [0..maxColumn], let i = (row, column)]

instance BitmapSearchable  BitmapArray
instance BitmapReflectable BitmapArray
