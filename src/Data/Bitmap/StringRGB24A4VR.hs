-- | This bitmap type is deprecated; use 'Data.Bitmap.String' instead
--
-- Bitmaps represented as strings
--
-- The module provides polymorphic support for representation of bitmaps as strings.
-- This module is designed to be most efficient with lazy bytestrings.

module Data.Bitmap.StringRGB24A4VR {-# DEPRECATED "Use Data.Bitmap.String instead" #-}
    ( BitmapStringRGB24A4VR
    ) where

import Data.Bitmap.StringRGB24A4VR.Internal
