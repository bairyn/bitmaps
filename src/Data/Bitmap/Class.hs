{-# LANGUAGE TypeFamilies, FlexibleContexts, TupleSections, ScopedTypeVariables #-}

module Data.Bitmap.Class
    ( Bitmap(..)
    , decodeCompleteFmt
    , decodeImageFmt
    , convertBitmap
    , completeDecodes, tryCBF_BMPIU, tryCBF_BMPIU64
    , imageDecodes
    ) where

import Control.Monad.Record
import Codec.String.Base16
import Codec.String.Base64
import Control.Applicative
import Control.Monad
import Data.Bitmap.Pixel
import Data.Bitmap.Types
import Data.Maybe
import Data.Serialize
import qualified Data.String.Class as S
import Data.Word
import Text.Printf

import Debug.Trace---------------------------------------------------

-- | Bitmap class
--
-- Pixels are indexed by (row, column), where (0, 0) represents the
-- upper-left-most corner of the bitmap.  Instances of this class
-- are not required to support empty bitmaps.
--
-- Default definitions for encoding and decoding are supplied.  Implementations
-- can define more efficient implementations.
class (Integral (BIndexType bmp)) => Bitmap bmp where
    type BIndexType bmp  -- ^ Integral type for each coordinate in an index

    depth           ::
        bmp
     -> Depth                                      -- ^ The color depth of the bitmap in bits
    dimensions      ::
        bmp
     -> Dimensions (BIndexType bmp)                -- ^ Return the width and height of the bitmap in pixels
    getPixel        ::
        bmp
     -> Coordinates (BIndexType bmp)
     -> Pixel                                      -- ^ Get a pixel; indexing starts at 0
    constructPixels ::
        (Coordinates (BIndexType bmp) -> Pixel)
     -> Dimensions (BIndexType bmp)
     -> bmp                                        -- ^ Construct a bitmap with a function that returns a pixel for each coordinate with the given dimensions
                                                   --
                                                   -- The function should return the same type of pixel for each coordinate.
                                                   --
                                                   -- Implementations are not required to call the function in any particular order, and are not even
                                                   -- required to guarantee that the function will be called for each pixel, which might be true for
                                                   -- a bitmap that is evaluated lazily as needed.

    encodeComplete  :: forall s. (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringPack s, S.StringString s, S.StringStrictByteString s, S.StringLazyByteString s, S.StringText s)
     => CompleteBitmapFormat -> bmp -> s           -- ^ Encode the bitmap 
    decodeComplete  :: forall s. (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringPack s, S.StringString s, S.StringStrictByteString s, S.StringLazyByteString s, S.StringText s)
     => s -> [(CompleteBitmapFormat, bmp)]         -- ^ Decode the bitmap; extra bytes after the end should be ignored
    encodeImage     :: forall s. (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringPack s, S.StringString s, S.StringStrictByteString s, S.StringLazyByteString s, S.StringText s)
     => ImageBitmapFormat -> bmp -> s           -- ^ Encode the bitmap; the meta-information is lost
    decodeImage     :: forall s. (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringPack s, S.StringString s, S.StringStrictByteString s, S.StringLazyByteString s, S.StringText s)
     => bmp -> s -> [(ImageBitmapFormat, bmp)]  -- ^ Decode the bitmap; the meta-information is taken from the given bitmap; extra bytes after the end should be ignored

    encodeComplete fmt b = case fmt of
        (CBF_BMPIU)   ->
            let header = S.fromStrictByteString . runPut $ do
                    -- Magic sequence
                    putWord8 (0x42 :: Word8)
                    putWord8 (0x4D :: Word8)

                    -- File size
                    putWord32le (fromIntegral $ 3 * width * height + padding * height + 0x0E + 40  :: Word32)

                    -- Reserved
                    putWord16le 0x0000
                    putWord16le 0x0000

                    -- Offset
                    putWord32le (0x0E + 40 :: Word32)

                    -- Bitmap information header; BITMAPINFOHEADER
                    -- header size
                    putWord32le (40 :: Word32)
                    -- width
                    putWord32le (fromIntegral width  :: Word32)
                    -- height
                    putWord32le (fromIntegral height  :: Word32)
                    -- number of color planes
                    putWord16le (1 :: Word16)
                    -- bits per pixel / depth
                    putWord16le (24 :: Word16)
                    -- compression
                    putWord32le (0 :: Word32)  -- no compression
                    -- image size
                    putWord32le (fromIntegral $ 3 * width * height + padding * height  :: Word32)
                    -- horizontal resolution; pixel per meter
                    putWord32le (3000 :: Word32)
                    -- vertical resolution; pixel per meter
                    putWord32le (3000 :: Word32)
                    -- number of colors
                    putWord32le (0 :: Word32)
                    -- number of important colors
                    putWord32le (0 :: Word32)
                image   = encodeImage IBF_RGB24A4 b
                padding = case 4 - ((3 * width * height) `mod` 4) of
                              4 -> 0
                              n -> n
            in  header `S.append` image

        (CBF_BMPIU64) -> encode64 . encodeComplete CBF_BMPIU $ b

        where (width, height) = dimensions b

    encodeImage fmt b = case fmt of
        (IBF_RGB24A4VF) ->
            let r' i@(row, column)
                    | column == maxColumn =
                        if row == 0
                            then
                                prep $ S.empty
                            else
                                prep $ r' (pred row, 0)
                    | otherwise           =
                        prep $ r' (row, succ column)
                    where pixel = b `getPixel` i
                          prep  = S.cons3 (S.toMainChar key $ red <: pixel) (S.toMainChar key $ green <: pixel) (S.toMainChar key $ blue <: pixel)
            in  r' (maxRow, 0)
        (IBF_RGB24A4)   ->
            let r' i@(row, column)
                    | column == maxColumn =
                        if row == maxRow
                            then
                                prep $ S.empty
                            else
                                prep $ r' (succ row, 0)
                    | otherwise           =
                        prep $ r' (row, succ column)
                    where pixel = b `getPixel` i
                          prep  = S.cons3 (S.toMainChar key $ red <: pixel) (S.toMainChar key $ green <: pixel) (S.toMainChar key $ blue <: pixel)
            in  r' (0, 0)

        where (width, height) = dimensions b
              maxRow          = abs . pred $ height
              maxColumn       = abs . pred $ width
              key             = S.keyStringConstruct :: s

    decodeComplete = catMaybes . (completeDecodes <*>) . pure
    decodeImage b s = catMaybes $ imageDecodes <*> pure b <*> pure s

decodeCompleteFmt :: (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringPack s, S.StringString s, S.StringStrictByteString s, S.StringLazyByteString s, S.StringText s, Bitmap bmp)
                      => CompleteBitmapFormat -> s -> Maybe bmp
decodeCompleteFmt fmt = (snd <$>) . listToMaybe . filter ((== fmt) . fst) . decodeComplete

decodeImageFmt :: (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringPack s, S.StringString s, S.StringStrictByteString s, S.StringLazyByteString s, S.StringText s, Bitmap bmp)
                   => ImageBitmapFormat -> bmp -> s -> Maybe bmp
decodeImageFmt fmt bmp = (snd <$>) . listToMaybe . filter ((== fmt) . fst) . decodeImage bmp

convertBitmap :: (Bitmap a, Bitmap b) => a -> b
convertBitmap b = constructPixels (\(row, column) -> getPixel b (fromIntegral row, fromIntegral column)) (let (width, height) = dimensions b in (fromIntegral width, fromIntegral height))

completeDecodes :: (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringPack s, S.StringString s, S.StringStrictByteString s, S.StringLazyByteString s, S.StringText s, Bitmap bmp)
                    => [s -> Maybe (CompleteBitmapFormat, bmp)]
completeDecodes =
    [ f CBF_BMPIU   tryCBF_BMPIU
    , f CBF_BMPIU64 tryCBF_BMPIU64
    ]
    where f fmt tryF = ((fmt, ) <$>) <$> tryF

tryCBF_BMPIU :: (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringPack s, S.StringString s, S.StringStrictByteString s, S.StringLazyByteString s, S.StringText s, Bitmap bmp)
                    => s -> Maybe bmp
tryCBF_BMPIU s = do
    let getImgInfo = do
            m0 <- getWord8
            m1 <- getWord8

            when (m0 /= 0x42 || m1 /= 0x4D) $ do
                fail "magic sequence is not that of BMP format"

            -- skip filesize 4, reserved 1, reserved, 1
            skip 8

            offset <- getWord32le

            -- get offset to image data
            let offset' = offset - 0x0E - 40

            when (offset' < 0) $ do
                fail $ printf "rewinding to image data at offset %d not supported" offset

            -- read DIB header
            headerSize <- getWord32le
            when (headerSize /= 40) $ do
                fail $ printf "header with size '%d' which is other than 40 is not supported" headerSize

            width  <- getWord32le
            height <- getWord32le
            numColorPlanes <- getWord16le
            when (numColorPlanes /= 1) $ do
                fail $ printf "numColorPlanes with value '%d' which is other than 1 is not supported" numColorPlanes
            bitsPerPixel <- getWord16le
            when (bitsPerPixel /= 24) $ do
                fail $ printf "bitsPerPixel with value '%d' which is other than 1 is not supported" bitsPerPixel
            compression <- getWord32le
            when (compression /= 0) $ do
                fail $ printf "compression with value '%d' which is other than 1 is not supported; needs to be uncompressed RGB" compression
            imageSize <- getWord32le
            let shouldBeImageSize = 3 * width * height + padding * height
                padding = case 4 - ((3 * width * height) `mod` 4) of
                              4 -> 0
                              n -> n
            when (imageSize /= shouldBeImageSize) $ do
                fail $ printf "imageSize was read to be '%d', but it should be '%d' since the dimensions of the bitmap are (%d, %d)" imageSize shouldBeImageSize width height
            -- skip horRes 4, verRes 4, usedColors 4, importantColors 4
            skip 16

            -- skip to image data
            when (offset' > 0) $ do
                skip $ fromIntegral offset'

            flip trace (return ()) $ printf "debug: width %d; height %d" width height

            -- get image data
            imgData <- getByteString (fromIntegral imageSize)

            return (width, height, imgData)
        isIBF_RGB24A4VF ((IBF_RGB24A4VF), _) = True
        isIBF_RGB24A4VF _                    = False

    --(width, height, imgData) <- either (const Nothing) Just . flip runGet (S.fromStrictByteString s) $ getImgInfo
    (width, height, imgData) <- either error Just . flip runGet (S.toStrictByteString s) $ getImgInfo  -- TODO use above
    let metaBitmap = constructPixels (const $ toPixelRGB leastIntensity) (fromIntegral width, fromIntegral height)
    decodeImageFmt IBF_RGB24A4VF metaBitmap $ imgData

tryCBF_BMPIU64 :: (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringPack s, S.StringString s, S.StringStrictByteString s, S.StringLazyByteString s, S.StringText s, Bitmap bmp)
                    => s -> Maybe bmp
tryCBF_BMPIU64 s = tryCBF_BMPIU =<< decode64 s

imageDecodes :: (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringPack s, S.StringString s, S.StringStrictByteString s, S.StringLazyByteString s, S.StringText s, Bitmap bmp)
                    => [bmp -> s -> Maybe (ImageBitmapFormat, bmp)]
imageDecodes =
    [ f IBF_RGB24A4VF tryIBF_RGB24A4VF
    ]
    where f fmt tryF = \bmp s -> (fmt, ) <$> tryF bmp s

tryIBF_RGB24A4VF :: (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringPack s, S.StringString s, S.StringStrictByteString s, S.StringLazyByteString s, S.StringText s, Bitmap bmp)
                    => bmp -> s -> Maybe bmp
tryIBF_RGB24A4VF metaBitmap s
    | S.length s < minLength = Nothing
    | otherwise              = Just $
        constructPixels pixelf dms
    where (width, height) = dimensions metaBitmap
          dms     = (fromIntegral width, fromIntegral height)
          padding = case 4 - ((3 * width * height) `mod` 4) of
                        4 -> 0
                        n -> n
          bytesPerPixel = 3
          bytesPerRow   = bytesPerPixel * width + padding
          minLength     = bytesPerRow * height
          pixelf (row, column) = let offset = fromIntegral $ bytesPerRow * (fromIntegral row) + bytesPerPixel * (fromIntegral column)
                                 in  (red   =: (S.toWord8 . fromJust $ s `S.index` (offset + 0)))
                                   . (green =: (S.toWord8 . fromJust $ s `S.index` (offset + 1)))
                                   . (blue  =: (S.toWord8 . fromJust $ s `S.index` (offset + 2)))
                                   $ toPixelRGB leastIntensity
