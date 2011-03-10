{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}

module Data.Bitmap.Croppable
    ( BitmapCroppable(..)
    ) where

import Data.Bitmap.Class
import Data.Bitmap.Types

-- | Class for bitmaps that can be rectangularly cropped into (possibly) smaller sizes
--
-- Using the functions of the 'Bitmap' class,
-- default functions are be defined for each of
-- these; of course, implementations are free
-- to write more efficient versions.
class (Bitmap bmp) => BitmapCroppable bmp where
    crop :: bmp -> Coordinates (BIndexType bmp) -> Dimensions (BIndexType bmp) -> bmp
        -- ^ Crop a bitmap with the given dimensions (third argument) from a position (second argument)
        --
        -- Implementations can but are not required to assume that the result will be a (not necessarily proper) subset of the super bitmap only.  Implementations should properly handle the case where the result is as large as the passed super bitmap.

    crop super (cropRow, cropColumn) = constructPixels (\(row, column) -> getPixel super (row + cropRow, column + cropColumn))

instance (Bitmap a) => BitmapCroppable a
