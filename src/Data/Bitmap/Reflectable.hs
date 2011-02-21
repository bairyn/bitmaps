{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}

module Data.Bitmap.Reflectable
    ( BitmapReflectable(..)
    ) where

import Data.Bitmap.Class

-- | Class for reflectable bitmaps
--
-- Using the functions of the 'Bitmap' class,
-- default functions are be defined for each of
-- these; of course, implementations are free
-- to write more efficient versions.
class (Bitmap bmp) => BitmapReflectable bmp where
    reflectVertically   :: bmp -> bmp
    reflectHorizontally :: bmp -> bmp

    reflectVertically b = constructPixels f dms
        where dms@(_, height) = dimensions b
              maxRow = abs . pred $ height
              f (r, c) = getPixel b (maxRow - r, c)
    reflectHorizontally b = constructPixels f dms
        where dms@(width, _)  = dimensions b
              maxColumn = abs . pred $ width
              f (r, c) = getPixel b (r,  maxColumn - c)

instance (Bitmap a) => BitmapReflectable a
