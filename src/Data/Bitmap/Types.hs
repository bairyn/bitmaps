{-# LANGUAGE DeriveDataTypeable #-}

module Data.Bitmap.Types
    ( Dimensions
    , Coordinates
    , Depth(..)
    , CompleteBitmapFormat(..)
    , ImageBitmapFormat(..)
    , mostLikelyMatchCBF
    , mostLikelyMatchIBF
    ) where

import Data.Data
import Data.Function (on)
import Data.List
import Data.Maybe (listToMaybe)

type Dimensions  i = (i, i)
type Coordinates i = (i, i)

-- | These are the color depths that the bitmap class supports
--
-- The depth does not necessarily indicate any particular order; instead the
-- components listed in the name indicate which components are contained in
-- each pixel.
--
-- The order in which the constructors are defined is significant; it determines
-- which match 'mostLikelyMatchCBF' will choose.
data Depth =
    Depth24RGB   -- ^ 24 bit pixel consisting of one pixel per component and lacking an alpha component
                 --
                 -- Each pixel of this type thus requires three bytes to be fully represented.  Implementations
                 -- are not required to pack the image data, however; they are free, for instance, to store each pixel
                 -- in 32-bit (4-byte) integers.
  | Depth32RGBA  -- ^ 32 bit pixel also consisting of one pixel per component but contains the alpha component
                 --
                 -- Four bytes are needed for pixels of this type to be fully represented.
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data)

-- | Formats that bitmaps that are instances of the 'Bitmap' class need to support; these formats include the necessary meta-information
data CompleteBitmapFormat =
    CBF_BMPIU    -- ^ Uncompressed BITMAPINFOHEADER BMP format (CompleteBitmapFormat_BitMaPInfoheaderUncompressed)

                 -- Due to being uncompressed, strings encoded in this format can grow large quite quickly.
                 -- This format is standard and widely supported, and can be written directly to a file, the extension
                 -- of which is typically ".bmp"
  | CBF_BMPIU64  -- ^ Same as 'CBF_BMPIU' except that that final result is base-64 encoded and is thus suitable for human-readable strings defining a bitmap (CompleteBitmapFormat_BitMaPInfoheaderUncompressed)
                 --
                 -- Like 'CBF_BMPIU', strings encoded in this format can become large quick quickly, and even about a third more so, due to the more restrictive range of values.
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data)

-- | Formats for raw image data that don't include information such as dimensions
data ImageBitmapFormat =
    IBF_RGB24A4VF  -- ^ Series of red, green, blue, red, etc. with a row alignment of 4, stored upside-down
  | IBF_RGB24A4    -- ^ Series of red, green, blue, red, etc. with a row alignment of 4
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data)

-- maybeList plus sort by format (it is ord)
mostLikelyMatchCBF :: [(CompleteBitmapFormat, a)] -> Maybe (CompleteBitmapFormat, a)
mostLikelyMatchCBF = listToMaybe . sortBy (compare `on` fst)

mostLikelyMatchIBF :: [(ImageBitmapFormat, a)] -> Maybe (ImageBitmapFormat, a)
mostLikelyMatchIBF = undefined
