{-# LANGUAGE DeriveDataTypeable #-}

module Data.Bitmap.Types
    ( Dimensions
    , Coordinates
    , Depth(..)
    , CompleteBitmapFormat(..)
    , ImageBitmapFormat(..)
    ) where

import Data.Data

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
    CBF_BMPIU     -- ^ Uncompressed BITMAPINFOHEADER BMP format (CompleteBitmapFormat_BitMaPInfoheaderUncompressed)
                  --
                  -- The image format of this format is 'IBF_BGR24A4VR'
                  --
                  -- Due to being uncompressed, strings encoded in this format can grow large quite quickly.
                  -- This format is standard and widely supported, and can be written directly to a file, the extension
                  -- of which is typically ".bmp"
                  --
                  -- This format is one possible format for "bmp" files.
  | CBF_BMPIU64   -- ^ Same as 'CBF_BMPIU' except that that final result is base-64 encoded and is thus suitable for human-readable strings defining a small bitmap (CompleteBitmapFormat_BitMaPInfoheaderUncompressed)
                  --
                  -- Like 'CBF_BMPIU', strings encoded in this format can become large quick quickly, and even about a third more so, due to the more restrictive range of values.
  | CBF_BMPIUZ64  -- Similar to CBF_BMPIU except that the encoded string is compressed before it is base-64 encoded
  | CBF_BMPIUU    -- ^ Similar to CBF_BMPIU, but internally the pixel data is stored in the 'IBF_BGRU32VR' format instead of 'IBF_BGR24A4VR'
                  --
                  -- This format is another common format of "bmp" files.
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data)

-- | Formats for raw image data that don't include information such as dimensions
data ImageBitmapFormat =
    IBF_IDRGB24Z64     -- ^ Series of red, green, and blue; the string is compressed and then base-64 encoded; the encoded string is prepended with an 'm' for simpler identification
  | IBF_IDBGR24R2RZ64  -- ^ Series of blue, green, and red, that is rotated two bytes right; the string is compressed and then base-64 encoded; the encoded string is prepended with a 'b' for simpler identification
  | IBF_IDBGR24HZH     -- ^ Series of red, green, and blue; the string is hex encoded, then compressed, then hex encoded again; the encoded string is prepended with with 'z'
  | IBF_IDRGB32Z64     -- ^ Series of unused byte, red, green, and blue; the string is compressed and then base-64 encoded; identified by an 'l' character
  | IBF_BGR24H         -- ^ Series of red, green, and blue, represented as a series of hexadecimal pairs
  | IBF_BGR24A4VR      -- ^ Series of blue, green, red, blue, etc. with a row alignment of 4, stored upside-down
  | IBF_BGRU32VR       -- ^ Series of blue, green, red, unused, blue, etc. stored upside-down (already aligned to a 4-byte boundary)
  | IBF_BGRU32         -- ^ Series of blue, green, red, unused, blue, etc. (already aligned to a 4-byte boundary)
  | IBF_RGB24A4VR      -- ^ Series of red, green, blue, red, etc. with a row alignment of 4, stored upside-down
  | IBF_RGB24A4        -- ^ Series of red, green, blue, red, etc. with a row alignment of 4
  | IBF_RGB32          -- ^ Series of unused byte, red, green, and blue
  | IBF_RGB32Z64       -- ^ Series of unused byte, red, green, and blue; the string is compressed and then base-64 encoded
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable, Data)
