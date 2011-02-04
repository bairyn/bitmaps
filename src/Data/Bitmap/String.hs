-- | Bitmaps represented as strings
--
-- The module provides polymorphic support for representation of bitmaps as strings.
-- This module is designed to be most efficient with lazy bytestrings.

module Data.Bitmap.String
    ( BitmapString
    ) where

import Data.Bitmap.String.Internal
