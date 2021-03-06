{-# LANGUAGE TemplateHaskell, TypeFamilies, ExistentialQuantification, TypeOperators, ScopedTypeVariables, TupleSections #-}

module Data.Bitmap.StringRGB32.Internal
    ( BitmapImageString(..)
    , BitmapStringRGB32(..), bmps_dimensions, bmps_data
    , encodeIBF_RGB32'
    , tryIBF_RGB32'
    , padByte
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Record                hiding (get)
import Data.Binary
import Data.Bitmap.Class
import Data.Bitmap.Pixel
import Data.Bitmap.Reflectable
import Data.Bitmap.Searchable
import Data.Bitmap.Types
import Data.Bitmap.Util                    hiding (padByte)
import Data.Bits
import qualified Data.ByteString      as B
import qualified Data.Serialize       as S
import qualified Data.String.Class    as S
import Data.Tagged
import Text.Printf

-- | Polymorphic container of a string
data BitmapImageString = forall s. (S.StringCells s) => BitmapImageString {_polyval_bitmapImageString :: s}

instance Eq BitmapImageString where
    a == b = case (a, b) of
        ((BitmapImageString sa), (BitmapImageString sb)) -> S.toStrictByteString sa == S.toStrictByteString sb
    a /= b = case (a, b) of
        ((BitmapImageString sa), (BitmapImageString sb)) -> S.toStrictByteString sa /= S.toStrictByteString sb

-- | A bitmap represented as a string, which contains a series of aligned rows, which themselves consist of a series of pixels stored in 4 bytes in which the most significant byte is unused (thus the rows are always aligned to a four-byte boundary)
data BitmapStringRGB32 = BitmapStringRGB32
    { _bmps_dimensions     :: (Int, Int)         -- ^ Width and height of the bitmap
    , _bmps_data           :: BitmapImageString  -- ^ Data stored in a string
    }

mkLabels [''BitmapStringRGB32]

instance Binary BitmapStringRGB32 where
    get   = pure BitmapStringRGB32 <*> get <*> (BitmapImageString <$> (get :: Get B.ByteString))
    put b = put (bmps_dimensions <: b) >> put (case bmps_data <: b of (BitmapImageString s) -> S.toLazyByteString s)

instance S.Serialize BitmapStringRGB32 where
    get   = pure BitmapStringRGB32 <*> S.get <*> (BitmapImageString <$> (S.get :: S.Get B.ByteString))
    put b = S.put (bmps_dimensions <: b) >> S.put (case bmps_data <: b of (BitmapImageString s) -> S.toLazyByteString s)

instance Bitmap BitmapStringRGB32 where
    type BIndexType BitmapStringRGB32 = Int
    type BPixelType BitmapStringRGB32 = PixelRGB

    depth = const Depth24RGB

    dimensions = (bmps_dimensions <:)

    getPixel b (row, column) =
        let (width, _) = bmps_dimensions <: b
            bytesPixel = 4
            bytesRow   = 4 * width
            offset     = bytesRow * row + bytesPixel * column
        in  case bmps_data <: b of
                (BitmapImageString s) ->
                    PixelRGB $ ((fromIntegral . S.toWord8 $ s `S.index` (offset + 1)) `shiftL` 16) .|.
                               ((fromIntegral . S.toWord8 $ s `S.index` (offset + 2)) `shiftL` 8)  .|.
                               ((fromIntegral . S.toWord8 $ s `S.index` (offset + 3)))

    constructPixels f dms@(width, height) = BitmapStringRGB32 dms . (BitmapImageString :: B.ByteString -> BitmapImageString) $
        S.unfoldrN (4 * width * height) getComponent (0 :: Int, 0 :: Int, 0 :: Int)
        where getComponent (row, column, orgb)
                  | orgb   > 3         =
                      getComponent (row, succ column, 0)
                  | column > maxColumn =
                      getComponent (succ row, 0, 0)
                  | row    > maxRow    =
                      Nothing
                  | otherwise =
                      let pixel = f (row, column)
                          componentGetter =
                              case orgb of
                                  0 -> const padCell
                                  1 -> untag' . S.toMainChar . (red   <:)
                                  2 -> untag' . S.toMainChar . (green <:)
                                  3 -> untag' . S.toMainChar . (blue  <:)
                                  _ -> undefined
                      in  Just (componentGetter pixel, (row, column, succ orgb))
              maxRow    = abs . pred $ height
              maxColumn = abs . pred $ width
              padCell   = untag' . S.toMainChar $ padByte
              untag' = untag :: Tagged B.ByteString a -> a

    imageEncoders = updateIdentifiableElements (map (second unwrapGenericBitmapSerializer) defaultImageEncoders) $
        [ (IBF_RGB32,    ImageEncoder $ encodeIBF_RGB32')
        ]

    imageDecoders = updateIdentifiableElements (map (second unwrapGenericBitmapSerializer) defaultImageDecoders) $
        [ (IBF_RGB32,    ImageDecoder $ tryIBF_RGB32')
        ]

encodeIBF_RGB32' :: (S.StringCells s) => BitmapStringRGB32 -> s
encodeIBF_RGB32' b = case (bmps_data <: b) of (BitmapImageString s) -> S.fromStringCells s

tryIBF_RGB32' :: (S.StringCells s) => BitmapStringRGB32 -> s -> Either String BitmapStringRGB32
tryIBF_RGB32' bmp s
    | S.length s < minLength = Left $ printf "Data.Bitmap.StringRGB32.Internal.tryIBF_RGB32': string is too small to contain the pixels of a bitmap with the dimensions of the passed bitmap, which are (%d, %d); the string is %d bytes long, but needs to be at least %d bytes long" (fromIntegral width  :: Integer) (fromIntegral height  :: Integer) (S.length s) minLength
    | otherwise              = Right $
        (bmps_data =: BitmapImageString s) bmp
    where (width, height) = bmps_dimensions <: bmp
          minLength       = 4 * width * height

padByte :: Word8
padByte = 0x00

instance BitmapSearchable BitmapStringRGB32 where
    findSubBitmapEqual super sub = case (bmps_data <: super, bmps_data <: sub) of
        ((BitmapImageString dataSuper), (BitmapImageString dataSub)) ->
            let (widthSuper, heightSuper) = bmps_dimensions <: super
                (widthSub,   heightSub)   = bmps_dimensions <: sub
                superBytesPerRow = 4 * widthSuper
                subBytesPerRow   = 4 * widthSub
                maxSuperRow      = heightSuper - heightSub
                maxSuperColumn   = widthSuper  - widthSub
                maxOffRow        = abs . pred $ heightSub

                r' i@(row, column)
                    | column > maxSuperColumn =
                        r' (succ row, 0)
                    | row    > maxSuperRow    =
                        Nothing
                    | matches 0               =
                        Just i
                    | otherwise               =
                        r' (row, succ column)
                    where superBaseIndex = row * superBytesPerRow + 4 * column
                          matches offRow
                              | offRow > maxOffRow = True
                              | (S.toStringCells :: S.StringCells s => s -> B.ByteString) (subStr (superBaseIndex + offRow * superBytesPerRow) subBytesPerRow dataSuper) /= (S.toStringCells :: (S.StringCells s) => s -> B.ByteString) (subStr (offRow * subBytesPerRow) subBytesPerRow dataSub) =
                                  False
                              | otherwise       = matches (succ offRow)
            in r'

instance BitmapReflectable BitmapStringRGB32
