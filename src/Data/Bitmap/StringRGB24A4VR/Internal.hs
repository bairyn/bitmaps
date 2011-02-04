{-# LANGUAGE TemplateHaskell, TypeFamilies, ExistentialQuantification, TypeOperators, ScopedTypeVariables, TupleSections #-}

module Data.Bitmap.StringRGB24A4VR.Internal
    ( BitmapImageString(..)
    , BitmapStringRGB24A4VR(..), bmps_dimensions, bmps_data
    , bytesPerRow
    , bitmapStringBytesPerRow
    , widthPadding
    , encodeIBF_RGB24A4VR'
    , tryIBF_RGB24A4VR'
    , padByte
    , imageSize
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Record
import Data.Binary
import Data.Bitmap.Class
import Data.Bitmap.Pixel
import Data.Bitmap.Reflectable
import Data.Bitmap.Searchable
import Data.Bitmap.Types
import Data.Bitmap.Util hiding (padByte)
import Data.Bits
import qualified Data.ByteString      as B
import qualified Data.Serialize       as S
import qualified Data.String.Class    as S
import Text.Printf

-- | Container for a string that represents a sequence of raw pixels lacking the alpha component and that is stored upside down
data BitmapImageString = forall s. (S.StringCells s) => BitmapImageString {_polyval_bitmapImageString :: s}

instance Eq BitmapImageString where
    a == b = case (a, b) of
        ((BitmapImageString sa), (BitmapImageString sb)) -> S.toStrictByteString sa == S.toStrictByteString sb
    a /= b = case (a, b) of
        ((BitmapImageString sa), (BitmapImageString sb)) -> S.toStrictByteString sa /= S.toStrictByteString sb

-- | A bitmap represented as a string
--
-- This is essentially the format of pixels
-- in the BMP format in which each row is aligned to
-- a four-byte boundry and each row contains a series of
-- RGB pixels.
--
-- This type is most efficient for programs interacting heavily with BMP files.
data BitmapStringRGB24A4VR = BitmapStringRGB24A4VR
    { _bmps_dimensions     :: (Int, Int)         -- ^ Width and height of the bitmap
    , _bmps_data           :: BitmapImageString  -- ^ Data stored in a string
    }

mkLabels [''BitmapStringRGB24A4VR]

instance Binary BitmapStringRGB24A4VR where
    get   = pure BitmapStringRGB24A4VR <*> get <*> (BitmapImageString <$> (get :: Get B.ByteString))
    put b = put (bmps_dimensions <: b) >> put (case bmps_data <: b of (BitmapImageString s) -> S.toLazyByteString s)

instance S.Serialize BitmapStringRGB24A4VR where
    get   = pure BitmapStringRGB24A4VR <*> S.get <*> (BitmapImageString <$> (S.get :: S.Get B.ByteString))
    put b = S.put (bmps_dimensions <: b) >> S.put (case bmps_data <: b of (BitmapImageString s) -> S.toLazyByteString s)

instance Bitmap BitmapStringRGB24A4VR where
    type BIndexType BitmapStringRGB24A4VR = Int
    type BPixelType BitmapStringRGB24A4VR = PixelRGB

    depth = const Depth24RGB

    dimensions = (bmps_dimensions <:)

    getPixel b (row, column) =
        let bytesPixel = 3
            bytesRow   = fst $ bitmapStringBytesPerRow b
            maxRow     = abs . pred . snd . dimensions $ b
            offset     = bytesRow * (maxRow - row) + bytesPixel * column
        in  case bmps_data <: b of
                (BitmapImageString s) ->
                    PixelRGB $ ((fromIntegral . S.toWord8 $ s `S.index` (offset    )) `shiftL` 16) .|.
                               ((fromIntegral . S.toWord8 $ s `S.index` (offset + 1)) `shiftL` 8)  .|.
                               ((fromIntegral . S.toWord8 $ s `S.index` (offset + 2)))

    constructPixels f dms@(width, height) = BitmapStringRGB24A4VR dms . (BitmapImageString :: B.ByteString -> BitmapImageString) $
        S.unfoldrN (imageSize dms) getComponent (0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int)
        where getComponent (row, column, orgb, paddingLeft)
                  | paddingLeft > 0    =
                      Just (padCell, (row, column, orgb, pred paddingLeft))
                  | orgb   > 2         =
                      getComponent (row, succ column, 0, 0)
                  | column > maxColumn =
                      getComponent (succ row, 0, 0, paddingSize)
                  | row    > maxRow    =
                      Nothing
                  | otherwise =
                      let pixel = f (row, column)
                          componentGetter =
                              case orgb of
                                  0 -> S.toMainChar key . (red   <:)
                                  1 -> S.toMainChar key . (green <:)
                                  2 -> S.toMainChar key . (blue  <:)
                                  _ -> undefined
                      in  Just (componentGetter pixel, (row, column, succ orgb, 0))
              maxRow      = abs . pred $ height
              maxColumn   = abs . pred $ width
              paddingSize = snd $ bytesPerRow width 3 4
              padCell     = S.toMainChar key $ padByte
              key         = S.keyStringCells :: B.ByteString

    imageEncoders = updateIdentifiableElements (map (second unwrapGenericBitmapSerializer) defaultImageEncoders) $
        [ (IBF_RGB24A4VR, ImageEncoder $ encodeIBF_RGB24A4VR')
        ]

    imageDecoders = updateIdentifiableElements (map (second unwrapGenericBitmapSerializer) defaultImageDecoders) $
        [ (IBF_RGB24A4VR, ImageDecoder $ tryIBF_RGB24A4VR')
        ]

encodeIBF_RGB24A4VR' :: (S.StringCells s) => BitmapStringRGB24A4VR -> s
encodeIBF_RGB24A4VR' b = case (bmps_data <: b) of (BitmapImageString s) -> S.fromStringCells s

tryIBF_RGB24A4VR' :: (S.StringCells s) => BitmapStringRGB24A4VR -> s -> Either String BitmapStringRGB24A4VR
tryIBF_RGB24A4VR' bmp s
    | S.length s < minLength = Left $ printf "Data.Bitmap.StringRGB24A4VR.Internal.tryIBF_RGB24A4VR': string is too small to contain the pixels of a bitmap with the dimensions of the passed bitmap, which are (%d, %d); the string is %d bytes long, but needs to be at least %d bytes long" (fromIntegral width  :: Integer) (fromIntegral height  :: Integer) (S.length s) minLength
    | otherwise              = Right $
        (bmps_data =: BitmapImageString s) bmp
    where (width, height) = bmps_dimensions <: bmp
          minLength       = imageSize (bmps_dimensions <: bmp)

bitmapStringBytesPerRow :: BitmapStringRGB24A4VR -> (Int, Int)
bitmapStringBytesPerRow b = bytesPerRow (fst $ bmps_dimensions <: b) 3 4

widthPadding :: Int -> String
widthPadding w = replicate (snd $ bytesPerRow w 3 4) $ S.toChar padByte

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

imageSize :: Dimensions Int -> Int
imageSize (width, height) = (fst $ bytesPerRow width 3 4) * height

instance BitmapSearchable BitmapStringRGB24A4VR where
    findSubBitmapEqual super sub = case (bmps_data <: super, bmps_data <: sub) of
        ((BitmapImageString dataSuper), (BitmapImageString dataSub)) ->
            let (widthSuper, heightSuper) = bmps_dimensions <: super
                (widthSub,   heightSub)   = bmps_dimensions <: sub
                superBytesPerRow = fst $ bitmapStringBytesPerRow super
                subBytesPerRow   = fst $ bitmapStringBytesPerRow sub
                maxSuperRow      = heightSuper - heightSub
                maxSuperColumn   = widthSuper  - widthSub
                maxOffRow        = abs . pred $ heightSub
                offRowSize       = subBytesPerRow - (snd $ bitmapStringBytesPerRow sub)

                r' (row, column)
                    | column > maxSuperColumn =
                        r' (succ row, 0)
                    | row    > maxSuperRow    =
                        Nothing
                    | matches 0               =
                        Just (maxSuperRow - row, column)
                    | otherwise               =
                        r' (row, succ column)
                    where superBaseIndex = row * superBytesPerRow + 3 * column
                          matches offRow
                              | offRow > maxOffRow
                                  = True
                              | (S.toStringCells :: S.StringCells s => s -> B.ByteString) (subStr (superBaseIndex + offRow * superBytesPerRow) offRowSize dataSuper) /=
                                (S.toStringCells :: S.StringCells s => s -> B.ByteString) (subStr (offRow * subBytesPerRow) offRowSize dataSub)
                                  = False
                              | otherwise
                                  = matches (succ offRow)
            in r'

instance BitmapReflectable BitmapStringRGB24A4VR
