{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Data.Bitmap.String.Internal
    ( BitmapString(..), bmps_data, bmps_dimensions, bmps_rowAlignment, bmps_redHead, bmps_alphaHead, bmps_paddingHead, bmps_paddingTail, bmps_rowFromTop, bmps_columnFromLeft, bmps_rowFromBeg, bmps_rowFromEnd, bmps_columnFromBeg, bmps_columnFromEnd
    , formatEq
    , defaultBSFormat
    , rowPadding
    , bytesPerPixel
    , rowPaddingBS
    , rgbOffsets
    , alphaOffset
    , pixelPart
    , imageSizeBS
    , constructBitmapStringFormatted
    , bitmapFmtBGR24A4VR
    , bitmapFmtRGB24A4VR
    , bitmapFmtRGB24A4
    , bitmapFmtRGB32
    , encodeBSFormat
    , encodeIBF_BGR24A4VR'
    , encodeIBF_RGB24A4VR'
    , encodeIBF_RGB24A4'
    , encodeIBF_RGB32'
    , tryBSFormat
    , tryIBF_BGR24A4VR'
    , tryIBF_RGB24A4VR'
    , tryIBF_RGB24A4'
    , tryIBF_RGB32'
    ) where

import Control.Applicative
--import Control.Arrow  -- See serializers part of 'BitmapString's 'Bitmap' instance
import Control.Monad.Record
import Data.Bits
import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Serialize       as S
import qualified Data.String.Class    as S
import Data.Bitmap.Class
import Data.Bitmap.Croppable
import Data.Bitmap.Pixel
import Data.Bitmap.Reflectable
import Data.Bitmap.Searchable
import Data.Bitmap.Types
import Data.Bitmap.Util
import Text.Printf

-- | A bitmap represented as a string or stored as bytes
--
-- By default, the RGB32 format (where the most significant byte, the
-- head-most one, is unused) is used.
--
-- The bitmap must be stored by pixels not separated by component.  Each
-- pixel must contain at least the red, blue, and green component, each one
-- byte wide (bytes are assumed to be octets), either in that order or
-- reversed.  There may be an alpha component either immediately before the
-- other three components or immediately after.  Thus there are four possible
-- arrangements of components for each pixel, which must be consistent for
-- every pixel.  Any amount of padding or unused bytes is permitted before each
-- pixel, but the amount must be fixed.  The same is true also after each
-- pixel.
--
-- This type is most efficient with lazy bytestrings.
data BitmapString = BitmapString
    { _bmps_data            :: S.GenString       -- ^ Bitmap data; it is assumed to be large enough
    , _bmps_dimensions      :: Dimensions (BIndexType BitmapString)  -- ^ Width and height of the data; 'bmps_rowFromBeg', etc. need to be taken account for the dimensions of the bitmap

    , _bmps_rowAlignment    :: Int             -- ^ Each row is aligned to this many bytes; when necessary, null bytes are added to each row
    , _bmps_redHead         :: Bool            -- ^ Whether the red component is first; if 'True', the order of the components is red, green, blue; otherwise, it is blue, green, red
    , _bmps_alphaHead       :: Maybe Bool      -- ^ If 'Nothing', then there is no alpha component; otherwise, if 'True', it is before the other three components (towards the head) / most significant / first, otherwise, it is after the other three components / towards the tail / least significant / last of the four components
    , _bmps_paddingHead     :: Int             -- ^ Number of unused bytes before each pixel
    , _bmps_paddingTail     :: Int             -- ^ Number of unused bytes after each pixel
    , _bmps_rowFromTop      :: Bool            -- ^ Is the first row at the top?
    , _bmps_columnFromLeft  :: Bool            -- ^ Is the first column in each row at the left?
    , _bmps_rowFromBeg      :: Int             -- ^ How many rows of data to skip from the beginning (from *first* row); used in cropping
    , _bmps_rowFromEnd      :: Int             -- ^ How many rows of data to skip from the end; used in cropping
    , _bmps_columnFromBeg   :: Int
    , _bmps_columnFromEnd   :: Int
    }

mkLabels [''BitmapString]

-- The data is serialized as a lazy bytestring
instance Binary BitmapString where
    get   = pure BitmapString <*> (S.toStringCells <$> (get :: Get B.ByteString)) <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
    put b = do
        put ((S.toStringCells :: S.GenString -> B.ByteString) $ bmps_data <: b)
        put $ bmps_dimensions     <: b
        put $ bmps_rowAlignment   <: b
        put $ bmps_redHead        <: b
        put $ bmps_alphaHead      <: b
        put $ bmps_paddingHead    <: b
        put $ bmps_paddingTail    <: b
        put $ bmps_rowFromTop     <: b
        put $ bmps_columnFromLeft <: b
        put $ bmps_rowFromBeg     <: b
        put $ bmps_rowFromEnd     <: b
        put $ bmps_columnFromBeg  <: b
        put $ bmps_columnFromEnd  <: b

-- The data is serialized as a lazy bytestring
instance S.Serialize BitmapString where
    get   = pure BitmapString <*> (S.toStringCells <$> (S.get :: S.Get B.ByteString)) <*> S.get <*> S.get <*> S.get <*> S.get <*> S.get <*> S.get <*> S.get <*> S.get <*> S.get <*> S.get <*> S.get <*> S.get
    put b = do
        S.put ((S.toStringCells :: S.GenString -> B.ByteString) $ bmps_data <: b)
        S.put $ bmps_dimensions     <: b
        S.put $ bmps_rowAlignment   <: b
        S.put $ bmps_redHead        <: b
        S.put $ bmps_alphaHead      <: b
        S.put $ bmps_paddingHead    <: b
        S.put $ bmps_paddingTail    <: b
        S.put $ bmps_rowFromTop     <: b
        S.put $ bmps_columnFromLeft <: b
        S.put $ bmps_rowFromBeg     <: b
        S.put $ bmps_rowFromEnd     <: b
        S.put $ bmps_columnFromBeg  <: b
        S.put $ bmps_columnFromEnd  <: b

formatEq :: BitmapString -> BitmapString -> Bool
formatEq a b
    | bmps_rowAlignment   <: a /= bmps_rowAlignment   <: b = False
    | bmps_redHead        <: a /= bmps_redHead        <: b = False
    | bmps_alphaHead      <: a /= bmps_alphaHead      <: b = False
    | bmps_paddingHead    <: a /= bmps_paddingHead    <: b = False
    | bmps_paddingTail    <: a /= bmps_paddingTail    <: b = False
    | bmps_rowFromTop     <: a /= bmps_rowFromTop     <: b = False
    | bmps_columnFromLeft <: a /= bmps_columnFromLeft <: b = False
    | bmps_rowFromBeg     <: a /= bmps_rowFromBeg     <: b = False
    | bmps_rowFromEnd     <: a /= bmps_rowFromEnd     <: b = False
    | bmps_columnFromBeg  <: a /= bmps_columnFromBeg  <: b = False
    | bmps_columnFromEnd  <: a /= bmps_columnFromEnd  <: b = False
    | otherwise                                            = True

-- | Default 'BitmapString' format
--
-- This is equivalent to 'IBF_BGRU32'
defaultBSFormat :: BitmapString
defaultBSFormat = BitmapString
    { _bmps_data           = error "data of defaultBSFormat is undefined"
    , _bmps_dimensions     = error "dimensions of defaultBSFormat is undefined"

    , _bmps_rowAlignment   = 1
    , _bmps_redHead        = False
    , _bmps_alphaHead      = Nothing
    , _bmps_paddingHead    = 0
    , _bmps_paddingTail    = 1
    , _bmps_rowFromTop     = True
    , _bmps_columnFromLeft = True
    , _bmps_rowFromBeg     = 0
    , _bmps_rowFromEnd     = 0
    , _bmps_columnFromBeg  = 0
    , _bmps_columnFromEnd  = 0
    }

-- | Return (rowSize, paddingSize) based on width, bytes per pixel, and alignment
--
-- Be careful when using the results of this function that you're actually using the right value.
rowPadding :: BIndexType BitmapString -> Int -> Int -> (Int, Int)
rowPadding bytes_per_pixel width alignment =
    (rawRowSize + off', off')
    where rawRowSize = bytes_per_pixel * width
          off        = rawRowSize `mod` alignment
          off'
              | off == 0  = 0
              | otherwise = alignment - off

bytesPerPixel :: BitmapString -> Int
bytesPerPixel bmp = (maybe 0 (const 1) $ bmps_alphaHead <: bmp) + (bmps_paddingHead <: bmp) + (bmps_paddingTail <: bmp) + 3

-- | Return (rowSize, paddingSize)
--
-- Be careful when using the results of this function that you're actually using the right value.
rowPaddingBS :: BitmapString -> (Int, Int)
rowPaddingBS bmp = rowPadding (bytesPerPixel bmp) (fst $ bmps_dimensions <: bmp) (bmps_rowAlignment <: bmp)

rgbOffsets :: BitmapString -> (Int, Int, Int)
rgbOffsets b
    | (Just True) <- bmps_alphaHead <: b
    , True        <- bmps_redHead   <: b
        = (ph + 1, ph + 2, ph + 3)
    | (Just True) <- bmps_alphaHead <: b
    , False       <- bmps_redHead   <: b
        = (ph + 3, ph + 2, ph + 1)
    | True        <- bmps_redHead   <: b
        = (ph + 0, ph + 1, ph + 2)
    | False       <- bmps_redHead   <: b
        = (ph + 2, ph + 1, ph + 0)
    | otherwise = error "Data.Bitmap.String.Internal.rgbOffsets: unexpected case"
    where ph = bmps_paddingHead <: b

alphaOffset :: BitmapString -> Maybe Int
alphaOffset b
    | (Just True)  <- bmps_alphaHead <: b
        = Just (ph)
    | (Just False) <- bmps_alphaHead <: b
        = Just (ph + 3)
    | (Nothing)    <- bmps_alphaHead <: b
        = Nothing
    | otherwise = error "Data.Bitmap.String.Internal.alphaOffset: unexpected case"
    where ph = bmps_paddingHead <: b

-- | Get part of a pixel as a cell of 'GenStringDefault'
--
-- The bitmap passed is only used for its format; its dimensions and data are not used.
-- This function doesn't return any alpha parts; for those it returns pad bytes, which are
-- zero.
pixelPart :: BitmapString -> BPixelType BitmapString -> Int -> S.StringCellChar S.GenStringDefault
pixelPart bmp pixel part
    | Just True <- bmps_alphaHead <: bmp
    , True      <- bmps_redHead   <: bmp
        = case baseIndex of
            1 -> r red
            2 -> r green
            3 -> r blue
            _ -> padCell
    | Just True <- bmps_alphaHead <: bmp
    , False     <- bmps_redHead   <: bmp
        = case baseIndex of
            3 -> r red
            2 -> r green
            1 -> r blue
            _ -> padCell
    | True      <- bmps_redHead   <: bmp
        = case baseIndex of
            0 -> r red
            1 -> r green
            2 -> r blue
            _ -> padCell
    | False     <- bmps_redHead   <: bmp
        = case baseIndex of
            2 -> r red
            1 -> r green
            0 -> r blue
            _ -> padCell
    | otherwise = error "Data.Bitmap.String.Internal.pixelPart: unexpected case"
    where ph        = bmps_paddingHead <: bmp
          baseIndex = part - ph
          r         = S.toMainChar key . (<: pixel)
          padCell   = S.toMainChar key $ padByte
          key       = S.keyStringCells :: S.GenStringDefault

imageSizeBS :: BitmapString -> Int
imageSizeBS b = (fst $ rowPaddingBS b) * (snd $ dimensions b)

-- | Construct a bitmap in the format of the meta bitmap passed
--
-- Only the format fields of the bitmap is used, so the data and dimensions of it can be 'undefined'.
--
-- The data in the new bitmap is what 'GenStringDefault' is aliased to.
constructBitmapStringFormatted :: BitmapString -> Dimensions (BIndexType BitmapString) -> (Coordinates (BIndexType BitmapString) -> BPixelType BitmapString) -> BitmapString
constructBitmapStringFormatted metaBitmap dms@(width, height) f =
    let maxRow          = abs . pred $ height
        maxColumn       = abs . pred $ width
        pixelSize       = bytesPerPixel metaBitmap
        (_, paddingSize) = rowPadding pixelSize width (bmps_rowAlignment <: metaBitmap)
        rowSize         = pixelSize * (width + bmps_columnFromBeg <: metaBitmap + bmps_columnFromEnd <: metaBitmap) + paddingSize
        newImageSize    = rowSize * (height + bmps_rowFromBeg <: metaBitmap + bmps_rowFromEnd <: metaBitmap)
        data_ :: S.GenStringDefault
        data_ = S.unfoldrN newImageSize getComponent (0 :: BIndexType BitmapString, 0 :: BIndexType BitmapString, 0 :: Int, rowSize * bmps_rowFromBeg <: metaBitmap :: Int)
        key   = S.keyStringCells :: S.GenStringDefault
        padCell     = S.toMainChar key $ padByte
        getComponent (row, column, part, paddingLeft)
            | paddingLeft > 0     =
                Just (padCell, (row, column, part, pred paddingLeft))
            | part   >= pixelSize =
                getComponent (row, succ column, 0, 0)
            | column >  maxColumn =
                getComponent (succ row, 0, 0, paddingSize)
            | row    == succ maxRow =
                getComponent (succ row, column, part, rowSize * bmps_rowFromEnd <: metaBitmap)
            | row    >  maxRow    =
                Nothing
            | otherwise =
                let pixel = f (row, column)
                in  Just (pixelPart metaBitmap pixel part, (row, column, succ part, 0))
    in  ((bmps_dimensions =: dms) . (bmps_data =: S.toStringCells data_)) $ metaBitmap

instance Bitmap BitmapString where
    type BIndexType BitmapString = Int
    type BPixelType BitmapString = PixelBGR

    depth = maybe Depth24RGB (const Depth32RGBA) . (bmps_alphaHead <:)

    dimensions bmp =
        let (width, height) = bmps_dimensions <: bmp
        in  (width - bmps_columnFromEnd <: bmp - bmps_columnFromBeg <: bmp, height - bmps_rowFromEnd <: bmp - bmps_rowFromBeg <: bmp)

    getPixel b (row, column) =
        let data_              = bmps_data <: b
            (width, height)    = dimensions b
            maxRow             = abs . pred $ height
            maxColumn          = abs . pred $ width
            rowSize            = fst $ rowPaddingBS b
            pixelSize          = bytesPerPixel b
            row'               = row    + bmps_rowFromBeg <: b
            column'            = column + bmps_columnFromBeg <: b
            rowOffset
                | bmps_rowFromTop <: b =
                    rowSize * row'
                | otherwise            =
                    rowSize * (maxRow - row')
            columnOffset
                | bmps_columnFromLeft <: b =
                    pixelSize * column'
                | otherwise                =
                    pixelSize * (maxColumn - column')
            offset             = rowOffset + columnOffset
            (offR, offG, offB) = rgbOffsets b
        in  PixelBGR
              $ ((fromIntegral . S.toWord8 $ data_ `S.index` (offset + offR)))
            .|. ((fromIntegral . S.toWord8 $ data_ `S.index` (offset + offG)) `shiftL` 8)
            .|. ((fromIntegral . S.toWord8 $ data_ `S.index` (offset + offB)) `shiftL` 16)

    constructPixels = flip $ constructBitmapStringFormatted defaultBSFormat

    convertInternalFormat metaBitmap imageBitmap
        | formatEq metaBitmap imageBitmap = imageBitmap
        | otherwise = constructBitmapStringFormatted metaBitmap (dimensions imageBitmap) (getPixel imageBitmap)

    -- FIXME: sometimes the bitmaps are upside-down
    {-
    imageEncoders = updateIdentifiableElements (map (second unwrapGenericBitmapSerializer) defaultImageEncoders) $
        [ (IBF_BGR24A4VR, ImageEncoder $ encodeIBF_BGR24A4VR')
        , (IBF_RGB24A4VR, ImageEncoder $ encodeIBF_RGB24A4VR')
        , (IBF_RGB24A4,   ImageEncoder $ encodeIBF_RGB24A4')
        , (IBF_RGB32,     ImageEncoder $ encodeIBF_RGB32')
        ]

    imageDecoders = updateIdentifiableElements (map (second unwrapGenericBitmapSerializer) defaultImageDecoders) $
        [ (IBF_BGR24A4VR, ImageDecoder $ tryIBF_BGR24A4VR')
        , (IBF_RGB24A4VR, ImageDecoder $ tryIBF_RGB24A4VR')
        , (IBF_RGB24A4,   ImageDecoder $ tryIBF_RGB24A4')
        , (IBF_RGB32,     ImageDecoder $ tryIBF_RGB32')
        ]
    -}

bitmapFmtBGR24A4VR :: BitmapString
bitmapFmtBGR24A4VR = BitmapString
    { _bmps_data           = error "Data.Bitmap.String.Internal.bitmapFmtBGR24A4VR: data of format is undefined"
    , _bmps_dimensions     = error "Data.Bitmap.String.Internal.bitmapFmtBGR24A4VR: dimensions of format is undefined"

    , _bmps_rowAlignment   = 4
    , _bmps_redHead        = False
    , _bmps_alphaHead      = Nothing
    , _bmps_paddingHead    = 0
    , _bmps_paddingTail    = 0
    , _bmps_rowFromTop     = False
    , _bmps_columnFromLeft = True
    , _bmps_rowFromBeg     = 0
    , _bmps_rowFromEnd     = 0
    , _bmps_columnFromBeg  = 0
    , _bmps_columnFromEnd  = 0
    }

bitmapFmtRGB24A4VR :: BitmapString
bitmapFmtRGB24A4VR = BitmapString
    { _bmps_data           = error "Data.Bitmap.String.Internal.bitmapFmtRGB24A4VR: data of format is undefined"
    , _bmps_dimensions     = error "Data.Bitmap.String.Internal.bitmapFmtBGR24A4VR: dimensions of format is undefined"

    , _bmps_rowAlignment   = 4
    , _bmps_redHead        = True
    , _bmps_alphaHead      = Nothing
    , _bmps_paddingHead    = 0
    , _bmps_paddingTail    = 0
    , _bmps_rowFromTop     = False
    , _bmps_columnFromLeft = True
    , _bmps_rowFromBeg     = 0
    , _bmps_rowFromEnd     = 0
    , _bmps_columnFromBeg  = 0
    , _bmps_columnFromEnd  = 0
    }

bitmapFmtRGB24A4 :: BitmapString
bitmapFmtRGB24A4 = BitmapString
    { _bmps_data           = error "Data.Bitmap.String.Internal.bitmapFmtRGB24A4: data of format is undefined"
    , _bmps_dimensions     = error "Data.Bitmap.String.Internal.bitmapFmtBGR24A4: dimensions of format is undefined"

    , _bmps_rowAlignment   = 4
    , _bmps_redHead        = True
    , _bmps_alphaHead      = Nothing
    , _bmps_paddingHead    = 0
    , _bmps_paddingTail    = 0
    , _bmps_rowFromTop     = True
    , _bmps_columnFromLeft = True
    , _bmps_rowFromBeg     = 0
    , _bmps_rowFromEnd     = 0
    , _bmps_columnFromBeg  = 0
    , _bmps_columnFromEnd  = 0
    }

bitmapFmtRGB32 :: BitmapString
bitmapFmtRGB32 = BitmapString
    { _bmps_data           = error "Data.Bitmap.String.Internal.bitmapFmtRGB32: data of format is undefined"
    , _bmps_dimensions     = error "Data.Bitmap.String.Internal.bitmapFmtBGR32: dimensions of format is undefined"

    , _bmps_rowAlignment   = 4
    , _bmps_redHead        = True
    , _bmps_alphaHead      = Nothing
    , _bmps_paddingHead    = 1
    , _bmps_paddingTail    = 0
    , _bmps_rowFromTop     = True
    , _bmps_columnFromLeft = True
    , _bmps_rowFromBeg     = 0
    , _bmps_rowFromEnd     = 0
    , _bmps_columnFromBeg  = 0
    , _bmps_columnFromEnd  = 0
    }

-- | Used by the encoders
encodeBSFormat :: (S.StringCells s) => BitmapString -> (BitmapString -> s)
encodeBSFormat bsFmt = S.toStringCells . (bmps_data <:) . convertInternalFormat bsFmt

encodeIBF_BGR24A4VR' :: (S.StringCells s) => BitmapString -> s
encodeIBF_BGR24A4VR' = encodeBSFormat bitmapFmtBGR24A4VR

encodeIBF_RGB24A4VR' :: (S.StringCells s) => BitmapString -> s
encodeIBF_RGB24A4VR' = encodeBSFormat bitmapFmtRGB24A4VR

encodeIBF_RGB24A4' :: (S.StringCells s) => BitmapString -> s
encodeIBF_RGB24A4' = encodeBSFormat bitmapFmtRGB24A4

encodeIBF_RGB32' :: (S.StringCells s) => BitmapString -> s
encodeIBF_RGB32' = encodeBSFormat bitmapFmtRGB32

-- | Used by the decoders
tryBSFormat :: (S.StringCells s) => String -> BitmapString -> (BitmapString -> s -> Either String BitmapString)
tryBSFormat identifier bsFmt  bmp s
    | S.length s < minLength = Left $ printf "Data.Bitmap.String.Internal.tryBSFormat: %s: string is too small to contain the pixels of a bitmap with the dimensions of the passed bitmap, which are (%d, %d); the string is %d " identifier
    | otherwise              = Right $
        (bmps_data       =: S.toStringCells s)
      . (bmps_dimensions =: dms)
      $ bsFmt
    where dms@(_, height) = dimensions bmp
          rowSize   = fst . rowPaddingBS . (bmps_dimensions =: dms) $ bsFmt
          minLength = rowSize * height

tryIBF_BGR24A4VR' :: (S.StringCells s) => BitmapString -> s -> Either String BitmapString
tryIBF_BGR24A4VR' = tryBSFormat "tryIBF_BGR24A4VR'" bitmapFmtBGR24A4VR

tryIBF_RGB24A4VR' :: (S.StringCells s) => BitmapString -> s -> Either String BitmapString
tryIBF_RGB24A4VR' = tryBSFormat "tryIBF_RGB24A4VR'" bitmapFmtRGB24A4VR

tryIBF_RGB24A4' :: (S.StringCells s) => BitmapString -> s -> Either String BitmapString
tryIBF_RGB24A4' = tryBSFormat "tryIBF_RGB24A4'" bitmapFmtRGB24A4

tryIBF_RGB32' :: (S.StringCells s) => BitmapString -> s -> Either String BitmapString
tryIBF_RGB32' = tryBSFormat "tryIBF_RGB32'" bitmapFmtRGB32

{-
-- TODO: work with crop as well
instance BitmapSearchable BitmapString where
    findSubBitmapEqual super sub_unformatted =
        let sub                       = convertInternalFormat super sub_unformatted
            (widthSuper, heightSuper) = bmps_dimensions <: super
            (widthSub,   heightSub)   = bmps_dimensions <: sub
            dataSuper                 = bmps_data <: super
            dataSub                   = bmps_data <: sub
            superRowSize              = fst $ rowPaddingBS super
            subRowSize                = fst $ rowPaddingBS sub
            superPixelSize            = bytesPerPixel super
            maxSuperRow               = heightSuper - heightSub
            maxSuperColumn            = widthSuper  - widthSub
            maxOffRow                 = abs . pred $ heightSub
            offRowSize                = subRowSize - (snd $ rowPaddingBS sub)

            r' (row, column)
                | column > maxSuperColumn =
                    r' (succ row, 0)
                | row    > maxSuperRow    =
                    Nothing
                | matches 0               =
                    Just (maxSuperRow - row, column)
                | otherwise               =
                    r' (row, succ column)
                where superBaseIndex = row * superRowSize + superPixelSize * column
                      matches offRow
                          | offRow > maxOffRow
                              = True
                          | subStr (superBaseIndex + offRow * superRowSize) offRowSize dataSuper /=
                            subStr (offRow * subRowSize) offRowSize dataSub
                              = False
                          | otherwise
                              = matches (succ offRow)
        in r'
-}

instance BitmapReflectable BitmapString where
    reflectVertically   b = (bmps_rowFromTop $: not)
                          . (bmps_rowFromBeg =: bmps_rowFromEnd <: b)
                          . (bmps_rowFromEnd =: bmps_rowFromBeg <: b)
                          $ b
    reflectHorizontally b = (bmps_columnFromLeft $: not)
                          . (bmps_columnFromBeg =: bmps_columnFromEnd <: b)
                          . (bmps_columnFromEnd =: bmps_columnFromBeg <: b)
                         $ b

instance BitmapCroppable BitmapString where
    crop bmp (row, column) (width, height) =
        (bmps_rowFromBeg    $: (+ row))
      . (bmps_rowFromEnd    $: (+ (bitmapHeight bmp - height - row)))
      . (bmps_columnFromBeg $: (+ column))
      . (bmps_columnFromEnd $: (+ (bitmapWidth  bmp - width  - column)))
      $ bmp
