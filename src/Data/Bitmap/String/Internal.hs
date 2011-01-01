{-# LANGUAGE TemplateHaskell, TypeFamilies, ExistentialQuantification, TypeOperators, ScopedTypeVariables #-}

module Data.Bitmap.String.Internal
    ( BitmapImageString(..)
    , BitmapString(..), bmps_dimensions, bmps_row_byte_align, bmps_alpha, bmps_pixel_bytes, bmps_data
    , bytesPerRow
    , bitmapStringBytesPerRow
    , padByte
    , bitmapStringFormat
    ) where

import Control.Applicative
import Control.Monad.Record
import Data.Bitmap.Class
import Data.Bitmap.Pixel
import Data.Bitmap.Reflectable
import Data.Bitmap.Searchable
import Data.Bitmap.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.List            as List
import Data.Serialize
import qualified Data.String.Class    as S
import Data.Word
import Text.Printf

-- | Container for a string that represents a sequence of raw pixels
data BitmapImageString = forall s. (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringPack s, S.StringString s, S.StringStrictByteString s, S.StringLazyByteString s, S.StringText s) =>
    BitmapImageString {_polyval_bitmapImageString :: s}

instance Eq BitmapImageString where
    a == b = case (a, b) of
        ((BitmapImageString sa), (BitmapImageString sb)) -> S.toLazyByteString sa == S.toLazyByteString sb
    a /= b = case (a, b) of
        ((BitmapImageString sa), (BitmapImageString sb)) -> S.toLazyByteString sa /= S.toLazyByteString sb

-- | A bitmap represented as a string
--
-- If alpha is 'True', each pixel is represented
-- as RGBA; otherwise, RGB.
data BitmapString = BitmapString
    { _bmps_dimensions     :: (Int, Int)         -- ^ Width and height of the bitmap
    , _bmps_row_byte_align :: Int                -- ^ The number of bytes per row is aligned to this value; if there are not enough bytes, padding bytes are added at the end of each row
                                                 --
                                                 -- For example, if this is set to 4 (which is required for the 'CBF_BMPIU' format),
                                                 -- and each row takes 15 bytes (if it were a 5-column wide RGB bitmap), one more byte would
                                                 -- be added to the end of each row so that it would be 4-byte aligned.
    , _bmps_alpha          :: Bool               -- ^ Whether the string contains an alpha component
    , _bmps_pixel_bytes    :: Int                -- ^ Bytes per pixel; if this is greater than the depth, then the first (left-most / head-most) bytes needs to be the unused ones
    , _bmps_data           :: BitmapImageString  -- ^ Data stored in a string
    }

mkLabels [''BitmapString]

instance Serialize BitmapString where
    get = pure BitmapString <*> get <*> get <*> get <*> get <*> (BitmapImageString <$> (get :: Get B.ByteString))
    put b = put (bmps_dimensions <: b) >> put (bmps_row_byte_align <: b) >> put (bmps_alpha <: b) >> put (bmps_pixel_bytes <: b) >> put (case bmps_data <: b of (BitmapImageString s) -> S.toLazyByteString s)

instance Bitmap BitmapString where
    type BIndexType BitmapString = Int

    depth b =
        if bmps_alpha <: b
            then Depth32RGBA
            else Depth24RGB

    dimensions = (bmps_dimensions <:)

    getPixel b (r, c)
        | bmps_alpha       <: b      =
              (red   =: getByte 0)
            . (green =: getByte 1)
            . (red   =: getByte 2)
            . (alpha =: getByte 3)
            $ toPixelRGBA leastIntensity
        | bmps_pixel_bytes <: b == 4 =
              (red   =: getByte 1)
            . (green =: getByte 2)
            . (red   =: getByte 3)
            $ toPixelRGB  leastIntensity
        | otherwise                  =
              (red   =: getByte 0)
            . (green =: getByte 1)
            . (red   =: getByte 2)
            $ toPixelRGB  leastIntensity
        where offset    = r * (fst . bitmapStringBytesPerRow) b + c * bmps_pixel_bytes <: b
              getByte i = maybe (error $
                                     printf "Bitmap.String.getPixel: index out of bounds; tried to access the first byte in pixel ('%d', '%d') offset by '%d'; dimensions of bitmap are ('%d', '%d')"
                                         r c i (fst $ bmps_dimensions <: b) (snd $ bmps_dimensions <: b)) id
                            $ (case bmps_data <: b of (BitmapImageString s) -> S.toWord8 <$> s `S.index` (offset + i))

    -- | Create a bitmap with a lazy bytestring
    constructPixels f (width, height) =
        BitmapString
            { _bmps_dimensions     = (width, height)
            , _bmps_row_byte_align = 4
            , _bmps_alpha          = isAlpha
            , _bmps_pixel_bytes    = if isAlpha then 4 else 3
            , _bmps_data           = BitmapImageString imgData
            }
        where maxRow    = abs . pred $ height
              maxColumn = abs . pred $ width
              imgDataF :: (BIndexType BitmapString, BIndexType BitmapString) -> B.ByteString
              imgDataF i@(row, column)
                  | column == maxColumn =
                        if row == maxRow
                            then prepPad . prep i $ S.empty
                            else prepPad . prep i $ imgDataF (succ row, 0)
                  | otherwise =
                        prep i $ imgDataF (row, succ column)
              imgData = imgDataF (0, 0)

              pixelf = f
              isAlpha  = case f (0, 0) of
                  (PixelRGB  _) -> False
                  (PixelBGR  _) -> False
                  (PixelRGBA _) -> True
                  (PixelBGRA _) -> True
              prep i
                  | isAlpha   = \s -> (S.toMainChar key $ red   <: pixelf i) `S.cons`
                                      (S.toMainChar key $ green <: pixelf i) `S.cons`
                                      (S.toMainChar key $ blue  <: pixelf i) `S.cons`
                                      (S.toMainChar key $ alpha <: pixelf i) `S.cons`
                                      s
                  | otherwise = \s -> (S.toMainChar key $ red   <: pixelf i) `S.cons`
                                      (S.toMainChar key $ green <: pixelf i) `S.cons`
                                      (S.toMainChar key $ blue  <: pixelf i) `S.cons`
                                      s
              prepPad = r' $ snd $ bytesPerRow width (if isAlpha then 4 else 3) 4
              r' 0    = \s -> s
              r' n    = \s -> (S.toMainChar key $ padByte) `S.cons` r' (pred n) s
              key :: B.ByteString
              key = S.keyStringConstruct

bitmapStringBytesPerRow :: BitmapString -> (Int, Int)
bitmapStringBytesPerRow b = bytesPerRow (fst $ bmps_dimensions <: b) (bmps_pixel_bytes <: b) (bmps_row_byte_align <: b)

-- | Return (rowSize, paddingSize) based on width, bytes per pixel, and alignment
bytesPerRow :: Int -> Int -> Int -> (Int, Int)
bytesPerRow width bytes_per_pixel alignment =
    (rawRowSize + off', off')
    where rawRowSize = bytes_per_pixel * width
          off        = rawRowSize `mod` alignment
          off'
              | off == 0  = 0
              | otherwise = alignment - off

padByte :: Word8
padByte = 0x00

-- | A relatively efficient means of converting the bitmap, passed as the second argument, to the format of the bitmap passed as the first argument
--
-- The dimensions of the bitmap (one being converted and is passed as the second argument)
-- are preserved, and the image data of the first bitmap is ignored.  The string type of
-- the data of the returned bitmap may be different from that of either of the other bitmaps.
--
-- This is particularly convenient to make checking pixels that are
-- consecutive in a row for equality more efficient.
bitmapStringFormat :: BitmapString -> BitmapString -> BitmapString
bitmapStringFormat metaBitmap bitmap
    | bmps_alpha          <: bitmap /= bmps_alpha          <: metaBitmap =
          -- Essentially the entire bitmap needs to be reconstructed
          flip constructPixels (bmps_dimensions <: bitmap) $ (if bmps_alpha <: metaBitmap then toPixelRGBA else  toPixelRGB) . getPixel bitmap
    | bmps_row_byte_align <: bitmap /= bmps_row_byte_align <: metaBitmap =
        -- Pixel representation is the same; reconstruct row by row
          (bmps_row_byte_align =: bmps_row_byte_align <: metaBitmap)
        . (bmps_data           =: case (bmps_data <: bitmap) of
              (BitmapImageString s) ->
                  let bitmapStringRows = r' (S.take (rowLength - paddingLength) s, S.drop rowLength s)
                          where r' ((Just row), (Just rest))
                                    | S.null row = []
                                    | otherwise  = row : r' (S.take (rowLength - paddingLength) rest, S.drop rowLength rest)
                                r' _             = []
                                (rowLength, paddingLength) = bitmapStringBytesPerRow bitmap
                      padding      = S.fromLazyByteString . B.pack . replicate paddingBytes $ padByte
                      paddingBytes = snd . bitmapStringBytesPerRow $ metaBitmap
                      s' = S.concat . List.intersperse padding $ bitmapStringRows
                      sPadEnd
                          | S.null s  = s'
                          | otherwise = s' `S.append` padding
                  in  BitmapImageString sPadEnd
          )
        $ bitmap
    | otherwise                                                          =
          -- Same format
          bitmap

instance BitmapSearchable BitmapString
{-
instance BitmapSearchable BitmapString where
    -- More efficient implementation of findSubBitmapEqual
    findSubBitmapEqual super sub_ = r' (0, 0)
        where sub = bitmapStringFormat super sub_
              r' i@(row, column)
                  | matches i           = Just i
                  | column >= maxColumn =
                      if row >= maxRow
                          then
                              Nothing
                          else
                              r' (succ row, 0)
                  | otherwise =
                      r' (row, succ column)

              (widthSuper, heightSuper) = dimensions super
              (widthSub,   heightSub)   = dimensions sub
              (maxRow,     maxColumn)   = (heightSuper - heightSub, widthSuper - widthSub)

              superImagedata            = bmps_data <: super
              subImagedata              = bmps_data <: sub
              superRowBytes             = fst $ bitmapStringBytesPerRow super
              superPixelBytes           = bmps_pixel_bytes <: super
              subRowBytes               = fst $ bitmapStringBytesPerRow sub

              maxOffRow                 = abs . pred $ heightSub
              maxOffColumn              = abs . pred $ widthSub

              {-
               -- Match row by row -- this seems to be really inefficient, probably due to the usage of S.take and S.drop.
              matches (superRow, superColumn) = r'' 0
                  where r'' offRow
                            | rowDiffers = False
                            | offRow >= (abs . pred $ heightSub) = True
                            | otherwise                          = r'' (succ offRow)
                            where rowDiffers = rowSuper /= rowSub
                                  rowSuper   = case superImagedata of
                                      (BitmapImageString ssuper) -> BitmapImageString . maybe S.empty id $ S.take subRowBytes =<< S.drop (superRowBytes * (superRow + offRow) + superPixelBytes * superColumn) ssuper
                                  rowSub     = case subImagedata   of
                                      (BitmapImageString ssub)   -> BitmapImageString . maybe S.empty id $ S.take subRowBytes =<< S.drop (subRowBytes * offRow) ssub
              -}
              -- Match pixel-by-pixel
              matches (superRow, superColumn) = r'' (0, 0)
                  where r'' (offRow, offColumn)
                            | pixelDiffers = False
                            | offRow >= maxOffRow = if offColumn >= maxColumn then True else r'' (succ offRow, 0)
                            | otherwise = r'' (offRow, succ offColumn)
                            where pixelDiffers = 
-}

instance BitmapReflectable BitmapString
