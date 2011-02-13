-- | Super module of bitmaps
--
-- For functions that expect two bitmaps or otherwise two parameters based on
-- two bitmaps (such as dimensions) to be passed, this library's convention
-- is to accept the "super" bitmap or the larger / main bitmap first and the
-- "sub" bitmap second.

module Data.Bitmap.BMP
    ( module Data.Bitmap.Array
    , module Data.Bitmap.Class
    , module Data.Bitmap.Foreign
    , module Data.Bitmap.Function
    , module Data.Bitmap.Pixel
    , module Data.Bitmap.Reflectable
    , module Data.Bitmap.Searchable
    , module Data.Bitmap.String
    , module Data.Bitmap.Types
    ) where

import Data.Bitmap.Array
import Data.Bitmap.Class
import Data.Bitmap.Foreign
import Data.Bitmap.Function
import Data.Bitmap.Pixel
import Data.Bitmap.Reflectable
import Data.Bitmap.Searchable
import Data.Bitmap.String
import Data.Bitmap.Types
