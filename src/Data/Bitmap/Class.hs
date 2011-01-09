{-# LANGUAGE TypeFamilies, PolymorphicComponents, TupleSections, FlexibleContexts, ScopedTypeVariables #-}

module Data.Bitmap.Class
    ( Bitmap(..)
    , convertBitmap

    -- * Polymorphic type wrappers
    , CompleteEncoder(..)
    , CompleteDecoder(..)
    , ImageEncoder(..)
    , ImageDecoder(..)
    , GenericBitmapSerializer(..)

    -- * Bitmap serialization
    , updateIdentifiableElements
    , defaultCompleteEncoders
    , encodeCBF_BMPIU
    , encodeCBF_BMPIU64
    , encodeCBF_BMPIUZ64
    , defaultCompleteDecoders
    , tryCBF_BMPIU
    , tryCBF_BMPIU64
    , tryCBF_BMPIUZ64
    , defaultImageEncoders
    , encodeIBF_RGB24A4
    , encodeIBF_RGB24A4VR
    , encodeIBF_RGB32
    , encodeIBF_RGB32Z64
    , defaultImageDecoders
    , tryIBF_RGB24A4
    , tryIBF_RGB24A4VR
    , tryIBF_RGB32
    , tryIBF_RGB32Z64
    , encodeComplete
    , decodeComplete
    , encodeImage
    , decodeImage
    , encodeCompleteFmt
    , decodeCompleteFmt
    , encodeImageFmt
    , decodeImageFmt
    , decodeImageDimensions
    , decodeImageDimensionsFmt
    ) where

import Codec.Compression.Zlib
import Codec.String.Base64
import Control.Monad.Record
import Control.Arrow
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bitmap.Pixel
import Data.Bitmap.Types
import Data.Bitmap.Util
import Data.Maybe
import qualified Data.String.Class as S
import Text.Printf

-- | Bitmap class
--
-- Pixels are indexed by (row, column), where (0, 0) represents the
-- upper-left-most corner of the bitmap.  Instances of this class
-- are not required to support empty bitmaps.
--
-- The encoding and decoding lists contain functions that can encode
-- and decode or return a string containing information about why
-- it could not be decoded in that format.  The order is important:
-- When a function tries multiple or any decoder, it will use or return
-- the one(s) closest to the head of the list.  There are lists
-- of generic functions that are defined by default.  Normally, if an
-- implementation of a bitmap type overrides the default instance,
-- it will only need to replace one or a few decoders, not touching
-- the rest of the default decoders or the order of the decoders;
-- thus the function 'updateIdentifiableElements' is defined and exported.
--
-- Instances *must* support every serialization format.
class (Integral (BIndexType bmp), Pixel (BPixelType bmp)) => Bitmap bmp where
    type BIndexType bmp  -- ^ Integral type for each coordinate in an index
    type BPixelType bmp  -- ^ Pixel type of structure

    depth                   :: bmp -> Depth
        -- ^ The color depth of the bitmap in bits

    dimensions              :: bmp -> Dimensions (BIndexType bmp)
        -- ^ Return the width and height of the bitmap in pixels

    getPixel                :: bmp -> Coordinates (BIndexType bmp) -> BPixelType bmp
        -- ^ Get a pixel; indexing starts at 0

    constructPixels         :: (Coordinates (BIndexType bmp) -> BPixelType bmp) -> Dimensions (BIndexType bmp) -> bmp
        -- ^ Construct a bitmap with a function that returns a pixel for each coordinate with the given dimensions
        --
        -- The function should return the same type of pixel for each coordinate.
        --
        -- Implementations are not required to call the function in any particular order, and are not even
        -- required to guarantee that the function will be called for each pixel, which might be true for
        -- a bitmap that is evaluated lazily as needed.

    completeEncoders :: [(CompleteBitmapFormat, CompleteEncoder bmp)]
        -- ^ Bitmap encoders; default definition is based on 'defaultCompleteEncoders'
    completeDecoders :: [(CompleteBitmapFormat, CompleteDecoder bmp)]
        -- ^ Bitmap decodes; extra bytes after the end should be ignored by them; default definition is based on 'defaultCompleteDecoders'
    imageEncoders    :: [(ImageBitmapFormat,    ImageEncoder    bmp)]
        -- ^ Bitmap encoders; the meta-information is lost; default definition is based on 'defaultImageEncoders'
    imageDecoders    :: [(ImageBitmapFormat,    ImageDecoder    bmp)]
        -- ^ Decode the bitmap; the meta-information from the given bitmap is used (see 'ImageDecoder'); default definition is based on 'defaultImageDecoders'

    completeEncoders = map (second unwrapGenericBitmapSerializer) defaultCompleteEncoders
    completeDecoders = map (second unwrapGenericBitmapSerializer) defaultCompleteDecoders
    imageEncoders    = map (second unwrapGenericBitmapSerializer) defaultImageEncoders
    imageDecoders    = map (second unwrapGenericBitmapSerializer) defaultImageDecoders

-- | Convert one bitmap type to another
convertBitmap :: (Bitmap a, Bitmap b) => a -> b
convertBitmap b = constructPixels (\(row, column) -> fromPixel $ getPixel b (fromIntegral row, fromIntegral column)) (let (width, height) = dimensions b in (fromIntegral width, fromIntegral height))

newtype CompleteEncoder bmp = CompleteEncoder {unwrapCompleteEncoder :: (S.StringCells s) => bmp -> s}
newtype CompleteDecoder bmp = CompleteDecoder {unwrapCompleteDecoder :: (S.StringCells s) => s -> Either String bmp}
newtype ImageEncoder    bmp = ImageEncoder    {unwrapImageEncoder    :: (S.StringCells s) => bmp -> s}
newtype ImageDecoder    bmp = ImageDecoder    {unwrapImageDecoder    :: (S.StringCells s) => bmp -> s -> Either String bmp}

newtype GenericBitmapSerializer s = GenericBitmapSerializer {unwrapGenericBitmapSerializer :: (Bitmap bmp) => s bmp}

-- | Update identifiable elements
--
-- 'updateIdentifiableElements' @orig new@ returns @orig@ with each matching
-- pair updated; extraneous replacements in @new@ are ignored.
updateIdentifiableElements :: (Eq k) => [(k, v)] -> [(k, v)] -> [(k, v)]
updateIdentifiableElements orig new = map (\(k, v) -> (k, maybe v id $ lookup k new)) orig

defaultCompleteEncoders :: [(CompleteBitmapFormat, GenericBitmapSerializer CompleteEncoder)]
defaultCompleteEncoders = 
    [ (CBF_BMPIU,    GenericBitmapSerializer $ CompleteEncoder $ encodeCBF_BMPIU)
    , (CBF_BMPIU64,  GenericBitmapSerializer $ CompleteEncoder $ encodeCBF_BMPIU64)
    , (CBF_BMPIUZ64, GenericBitmapSerializer $ CompleteEncoder $ encodeCBF_BMPIUZ64)
    ]

encodeCBF_BMPIU :: (S.StringCells s, Bitmap bmp) => bmp -> s
encodeCBF_BMPIU b =
    let (width, height) = dimensions b
        header = S.fromLazyByteString . runPut $ do
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
        image   = encodeImageFmt IBF_RGB24A4VR b
        padding = case 4 - ((3 * width) `mod` 4) of
                      4 -> 0
                      n -> n
    in  header `S.append` image

encodeCBF_BMPIU64 :: (S.StringCells s, Bitmap bmp) => bmp -> s
encodeCBF_BMPIU64 = encode64 . encodeCompleteFmt CBF_BMPIU

encodeCBF_BMPIUZ64 :: (S.StringCells s, Bitmap bmp) => bmp -> s
encodeCBF_BMPIUZ64 = encode64 . S.fromStringCells . compress . encodeCompleteFmt CBF_BMPIU

defaultCompleteDecoders :: [(CompleteBitmapFormat, GenericBitmapSerializer CompleteDecoder)]
defaultCompleteDecoders =
    [ (CBF_BMPIU,    GenericBitmapSerializer $ CompleteDecoder $ tryCBF_BMPIU)
    , (CBF_BMPIU64,  GenericBitmapSerializer $ CompleteDecoder $ tryCBF_BMPIU64)
    , (CBF_BMPIUZ64, GenericBitmapSerializer $ CompleteDecoder $ tryCBF_BMPIUZ64)
    ]

tryCBF_BMPIU :: (S.StringCells s, Bitmap bmp) => s -> Either String bmp
tryCBF_BMPIU s = do
    let getImgInfo = do
            m0 <- getWord8
            m1 <- getWord8

            when (m0 /= 0x42 || m1 /= 0x4D) $ do
                fail "magic sequence is not that of BMP format"

            -- skip filesize 4, reserved 2, reserved, 2
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
            let shouldBeImageSize = (3 * width + padding) * height
                padding = case (3 * width) `mod` 4 of
                              0 -> 0
                              n -> 4 - n
            when (imageSize /= shouldBeImageSize) $ do
                fail $ printf "imageSize was read to be '%d', but it should be '%d' since the dimensions of the bitmap are (%d, %d)" imageSize shouldBeImageSize width height
            -- skip horRes 4, verRes 4, usedColors 4, importantColors 4
            skip 16

            -- skip to image data
            when (offset' > 0) $ do
                skip $ fromIntegral offset'

            -- get image data
            imgData <- getLazyByteString (fromIntegral imageSize)

            return (width, height, imgData)

    (width, height, imgData) <- tablespoon . flip runGet (S.toLazyByteString s) $ getImgInfo
    let metaBitmap = constructPixels (const leastIntensity) (fromIntegral width, fromIntegral height)
    decodeImageFmt IBF_RGB24A4VR metaBitmap $ imgData

tryCBF_BMPIU64 :: (S.StringCells s, Bitmap bmp) => s -> Either String bmp
tryCBF_BMPIU64 s = tryCBF_BMPIU =<< (maybe (Left "Data.Bitmap.Class.tryCBF_BMPIU64: not a valid sequence of characters representing a base-64 encoded string") Right $ decode64 s)

tryCBF_BMPIUZ64 :: (S.StringCells s, Bitmap bmp) => s -> Either String bmp
tryCBF_BMPIUZ64 s = tryCBF_BMPIU =<< tablespoon . decompress . S.toStringCells =<< (maybe (Left "Data.Bitmap.Class.tryCBF_BMPIUZ64: not a valid sequence of characters representing a base-64 encoded string") Right $ decode64 s)

defaultImageEncoders :: [(ImageBitmapFormat, GenericBitmapSerializer ImageEncoder)]
defaultImageEncoders =
    [ (IBF_RGB24A4,   GenericBitmapSerializer $ ImageEncoder $ encodeIBF_RGB24A4)
    , (IBF_RGB24A4VR, GenericBitmapSerializer $ ImageEncoder $ encodeIBF_RGB24A4VR)
    , (IBF_RGB32,     GenericBitmapSerializer $ ImageEncoder $ encodeIBF_RGB32)
    , (IBF_RGB32Z64,  GenericBitmapSerializer $ ImageEncoder $ encodeIBF_RGB32Z64)
    ]

encodeIBF_RGB24A4 :: forall s bmp. (S.StringCells s, Bitmap bmp) => bmp -> s
encodeIBF_RGB24A4 b =
    let r' i@(row, column)
            | column > maxColumn =
                padding `S.append`
                r' (succ row, 0)
            | row    > maxRow    =
                S.empty
            | otherwise          =
                S.cons3
                    (S.toMainChar key $ red   <: pixel)
                    (S.toMainChar key $ green <: pixel)
                    (S.toMainChar key $ blue  <: pixel)
                    $ r' (row, succ column)
            where pixel = b `getPixel` i
        paddingSize = case 4 - ((3 * width) `mod` 4) of
                          4 -> 0
                          n -> n
        padding  = S.fromStringCells $ replicate (fromIntegral paddingSize) (S.toChar (0x00 :: Word8))
    in  r' (0, 0)
    where (width, height) = dimensions b
          maxRow          = abs . pred $ height
          maxColumn       = abs . pred $ width
          key             = S.keyStringCells :: s

encodeIBF_RGB24A4VR :: forall s bmp. (S.StringCells s, Bitmap bmp) => bmp -> s
encodeIBF_RGB24A4VR b =
    let r' i@(row, column)
            | column > maxColumn =
                padding `S.append`
                r' (pred row, 0)
            | row    < 0         =
                S.empty
            | otherwise          =
                S.cons3
                    (S.toMainChar key $ red   <: pixel)
                    (S.toMainChar key $ green <: pixel)
                    (S.toMainChar key $ blue  <: pixel)
                    $ r' (row, succ column)
            where pixel = b `getPixel` i
        paddingSize = case 4 - ((3 * width) `mod` 4) of
                          4 -> 0
                          n -> n
        padding  = S.fromStringCells $ replicate (fromIntegral paddingSize) (S.toChar (0x00 :: Word8))
    in  r' (maxRow, 0)
    where (width, height) = dimensions b
          maxRow          = abs . pred $ height
          maxColumn       = abs . pred $ width
          key             = S.keyStringCells :: s

encodeIBF_RGB32 :: forall s bmp. (S.StringCells s, Bitmap bmp) => bmp -> s
encodeIBF_RGB32 b =
    let r' i@(row, column)
            | column > maxColumn =
                r' (succ row, 0)
            | row    > maxRow    =
                S.empty
            | otherwise          =
                S.cons4
                    padCell
                    (S.toMainChar key $ red   <: pixel)
                    (S.toMainChar key $ green <: pixel)
                    (S.toMainChar key $ blue  <: pixel)
                    $ r' (row, succ column)
            where pixel = b `getPixel` i
    in  r' (0, 0)
    where (width, height) = dimensions b
          maxRow          = abs . pred $ height
          maxColumn       = abs . pred $ width
          padCell         = S.toMainChar key $ (0x00 :: Word8)
          key             = S.keyStringCells :: s

encodeIBF_RGB32Z64 :: forall s bmp. (S.StringCells s, Bitmap bmp) => bmp -> s
encodeIBF_RGB32Z64 = encode64 . S.fromStringCells . compress . encodeIBF_RGB32

defaultImageDecoders :: [(ImageBitmapFormat, GenericBitmapSerializer ImageDecoder)]
defaultImageDecoders =
    [ (IBF_RGB24A4,   GenericBitmapSerializer $ ImageDecoder $ tryIBF_RGB24A4)
    , (IBF_RGB24A4VR, GenericBitmapSerializer $ ImageDecoder $ tryIBF_RGB24A4VR)
    , (IBF_RGB32,     GenericBitmapSerializer $ ImageDecoder $ tryIBF_RGB32)
    , (IBF_RGB32Z64,  GenericBitmapSerializer $ ImageDecoder $ tryIBF_RGB32Z64)
    ]

tryIBF_RGB24A4 :: (S.StringCells s, Bitmap bmp) => bmp -> s -> Either String bmp
tryIBF_RGB24A4 metaBitmap s
    | S.length s < minLength = Left $ printf "Data.Bitmap.Class.tryIBF_RGB24A4: string is too small to contain the pixels of a bitmap with the dimensions of the passed bitmap, which are (%d, %d); the string is %d bytes long, but needs to be at least %d bytes long" (fromIntegral width  :: Integer) (fromIntegral height  :: Integer) (S.length s) minLength
    | otherwise              = Right $
        constructPixels pixelf dms
    where (width, height) = dimensions metaBitmap
          dms     = (fromIntegral width, fromIntegral height)
          padding = case 4 - ((3 * width) `mod` 4) of
                        4 -> 0
                        n -> n
          bytesPerPixel = 3
          bytesPerRow   = bytesPerPixel * width + padding
          minLength     = fromIntegral $ bytesPerRow * height
          pixelf (row, column) = let offset = fromIntegral $ bytesPerRow * (fromIntegral row) + bytesPerPixel * (fromIntegral column)
                                 in  (red   =: (S.toWord8 $ s `S.index` (offset + 0)))
                                   . (green =: (S.toWord8 $ s `S.index` (offset + 1)))
                                   . (blue  =: (S.toWord8 $ s `S.index` (offset + 2)))
                                   $ leastIntensity

tryIBF_RGB24A4VR :: (S.StringCells s, Bitmap bmp) => bmp -> s -> Either String bmp
tryIBF_RGB24A4VR metaBitmap s
    | S.length s < minLength = Left $ printf "Data.Bitmap.Class.tryIBF_RGB24A4VR: string is too small to contain the pixels of a bitmap with the dimensions of the passed bitmap, which are (%d, %d); the string is %d bytes long, but needs to be at least %d bytes long" (fromIntegral width  :: Integer) (fromIntegral height  :: Integer) (S.length s) minLength
    | otherwise              = Right $
        constructPixels pixelf dms
    where (width, height) = dimensions metaBitmap
          dms     = (fromIntegral width, fromIntegral height)
          padding = case 4 - ((3 * width) `mod` 4) of
                        4 -> 0
                        n -> n
          bytesPerPixel = 3
          bytesPerRow   = bytesPerPixel * width + padding
          minLength     = fromIntegral $ bytesPerRow * height
          maxRow        = abs . pred $ height
          pixelf (row, column) = let offset = fromIntegral $ bytesPerRow * (fromIntegral $ maxRow - row) + bytesPerPixel * (fromIntegral column)
                                 in  (red   =: (S.toWord8 $ s `S.index` (offset + 0)))
                                   . (green =: (S.toWord8 $ s `S.index` (offset + 1)))
                                   . (blue  =: (S.toWord8 $ s `S.index` (offset + 2)))
                                   $ leastIntensity

tryIBF_RGB32 :: (S.StringCells s, Bitmap bmp) => bmp -> s -> Either String bmp
tryIBF_RGB32 metaBitmap s
    | S.length s < minLength = Left $ printf "Data.Bitmap.Class.tryIBF_RGB32: string is too small to contain the pixels of a bitmap with the dimensions of the passed bitmap, which are (%d, %d); the string is %d bytes long, but needs to be at least %d bytes long" (fromIntegral width  :: Integer) (fromIntegral height  :: Integer) (S.length s) minLength
    | otherwise              = Right $
        constructPixels pixelf dms
    where (width, height) = dimensions metaBitmap
          dms     = (fromIntegral width, fromIntegral height)
          bytesPerPixel = 4
          bytesPerRow   = bytesPerPixel * width
          minLength     = fromIntegral $ bytesPerRow * height
          pixelf (row, column) = let offset = fromIntegral $ bytesPerRow * (fromIntegral row) + bytesPerPixel * (fromIntegral column)
                                 in  (red   =: (S.toWord8 $ s `S.index` (offset + 1)))
                                   . (green =: (S.toWord8 $ s `S.index` (offset + 2)))
                                   . (blue  =: (S.toWord8 $ s `S.index` (offset + 3)))
                                   $ leastIntensity

tryIBF_RGB32Z64 :: (S.StringCells s, Bitmap bmp) => bmp -> s -> Either String bmp
tryIBF_RGB32Z64 metaBitmap s = tryIBF_RGB32 metaBitmap =<< tablespoon . decompress . S.toStringCells =<< (maybe (Left "Data.Bitmap.Class.tryIBF_RGB32Z64: not a valid sequence of characters representing a base-64 encoded string") Right $ decode64 s)

-- | Encode a bitmap
--
-- An implementation can choose the most efficient or appropriate
-- format by placing its encoder first in its list of encoders.
encodeComplete :: (S.StringCells s, Bitmap bmp) => bmp -> s
encodeComplete
    | null encoders = error $ printf "encodeComplete: implementation defines no encoders"
    | otherwise     = unwrapCompleteEncoder . snd . head $ encoders
    where encoders = completeEncoders

-- | Decode a bitmap
--
-- The result of first decoder of the implementation that succeeds
-- will be returned.  If none succeed, 'Nothing' is returned.
decodeComplete :: (S.StringCells s, Bitmap bmp) => s -> Maybe (CompleteBitmapFormat, bmp)
decodeComplete s = listToMaybe . catMaybes . map (\(fmt, decoder) -> either (const Nothing) (Just . (fmt, )) $ unwrapCompleteDecoder decoder s) $ completeDecoders

-- | Encode the pixels of a bitmap
--
-- An implementation can choose the most efficient or appropriate
-- format by placing its encoder first in its list of encoders.
encodeImage :: (S.StringCells s, Bitmap bmp) => bmp -> s
encodeImage
    | null encoders = error $ printf "encodeImage: implementation defines no encoders"
    | otherwise     = unwrapImageEncoder . snd . head $ encoders
    where encoders = imageEncoders

-- | Decode the pixels of a bitmap
--
-- The result of first decoder of the implementation that succeeds
-- will be returned.  If none succeed, 'Nothing' is returned.
decodeImage :: (S.StringCells s, Bitmap bmp) => bmp -> s -> Maybe (ImageBitmapFormat, bmp)
decodeImage bmp s = listToMaybe . catMaybes . map (\(fmt, decoder) -> either (const Nothing) (Just . (fmt, )) $ unwrapImageDecoder decoder bmp s) $ imageDecoders

serializeFmt :: (Eq a, Show a) => [(a, b)] -> String -> (a -> b)
serializeFmt serializers noHandlerErrorFmtStr = \fmt -> case lookup fmt serializers of
    (Just f)  -> f
    (Nothing) -> error $
        printf
            noHandlerErrorFmtStr
            (show fmt)

-- | Encode a bitmap in a particular format
encodeCompleteFmt :: (S.StringCells s, Bitmap bmp) => CompleteBitmapFormat -> bmp -> s
encodeCompleteFmt = unwrapCompleteEncoder . serializeFmt completeEncoders
    "encodeCompleteFmt: Bitmap instance did not define handler for encoding format '%s'; does it use 'updateIdentifiableElements' with 'defaultCompleteEncoders' properly?"

-- | Decode a bitmap in a particular format
decodeCompleteFmt :: (S.StringCells s, Bitmap bmp) => CompleteBitmapFormat -> s -> Either String bmp
decodeCompleteFmt = unwrapCompleteDecoder . serializeFmt completeDecoders
    "decodeCompleteFmt: Bitmap instance did not define handler for decoding '%s'; does it use 'updateIdentifiableElements' with 'defaultCompleteDecoders' properly?"

-- | Encode the pixels of a bitmap in a particular format
encodeImageFmt :: (S.StringCells s, Bitmap bmp) => ImageBitmapFormat -> bmp -> s
encodeImageFmt    = unwrapImageEncoder . serializeFmt imageEncoders
    "encodeImageFmt: Bitmap instance did not define handler for encoding format '%s'; does it use 'updateIdentifiableElements' with 'defaultImageEncoders' properly?"

-- | Decode the pixels of a bitmap in a particular format
decodeImageFmt :: (S.StringCells s, Bitmap bmp) => ImageBitmapFormat -> bmp -> s -> Either String bmp
decodeImageFmt    = unwrapImageDecoder . serializeFmt imageDecoders
    "decodeImageFmt: Bitmap instance did not define handler for decoding format '%s'; does it use 'updateIdentifiableElements' with 'defaultImageDecoders' properly?"

-- | Decode an image with the given dimensions
--
-- This is only guaranteed to work on implementations and formats that only
-- need dimensions in addition to the raw pixel data.  This is convenient
-- because most often the dimensions are all that is needed.
--
-- Currently, this function works by constructing a bitmap with the given dimensions
-- and with each pixel set to the least intensity.  Thus it is significantly more efficient
-- if this is used with a bitmap that doesn't strictly evaluate the entire pixel data when the structure
-- is first constructed (not necessarily when any pixel is accessed) (currently
-- none of the bitmap types exported in this library are so strict), as the
-- bitmap will not need to be fully evaluated; only the dimensions will be used.
decodeImageDimensions :: (S.StringCells s, Bitmap bmp) => Dimensions (BIndexType bmp) -> s -> Maybe (ImageBitmapFormat, bmp)
decodeImageDimensions dms = decodeImage (constructPixels (const leastIntensity) dms)

-- | Decode an image with the given dimensions as 'decodeImageDimensions' does it, but in a specific format
decodeImageDimensionsFmt :: (S.StringCells s, Bitmap bmp) => ImageBitmapFormat -> Dimensions (BIndexType bmp) -> s -> Either String bmp
decodeImageDimensionsFmt fmt dms = decodeImageFmt fmt (constructPixels (const leastIntensity) dms)
