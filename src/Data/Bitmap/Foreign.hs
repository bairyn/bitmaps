{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module Data.Bitmap.Foreign
    ( FBBitmapBase
    , BitmapForeign(..)
    ) where

import Control.Monad
import Control.Monad.Record
import qualified Data.Bitmap      as FB
import qualified Data.Bitmap.IO   as FB
import Data.Bitmap.Class
import Data.Bitmap.Pixel
import Data.Bitmap.Reflectable
import Data.Bitmap.Searchable
import Data.Bitmap.Types
import Foreign (unsafePerformIO)
import Foreign.Storable
import Text.Printf

type FBBitmapBase = FB.Bitmap

-- | The foreign bitmap as defined by the "bitmap" package
--
-- For more information see documentation of the "bitmap" package.
-- NB: this type is actually a reference to a memory location, introducing
-- possible issues with concurrency and referential transparency.
newtype BitmapForeign = BitmapForeign {unwrapBitmapForeign :: FBBitmapBase PixelComponent}

instance Bitmap BitmapForeign where
    type BIndexType BitmapForeign = Int

    depth (BitmapForeign b) = unsafePerformIO . FB.withBitmap b $ \_ numComponents _ _ -> case numComponents of
        3 -> return Depth24RGB
        4 -> return Depth32RGBA
        _ -> return $ error $ printf "Bitmap.ForeignBitmap.depth: invalid numConponents value: %d" numComponents

    dimensions (BitmapForeign b) = unsafePerformIO . FB.withBitmap b $ \dms _ _ _ -> return dms

    getPixel (BitmapForeign b) (row, column) = unsafePerformIO . FB.withBitmap b $ \(w, _) numComponents padding ptr -> do
        let bytesPixel = numComponents
            bytesRow   = bytesPixel * w + padding
            offset     = bytesRow * row + bytesPixel * column
        if numComponents == 3
            then do
                (thisRed   :: PixelComponent) <- peekByteOff ptr offset
                (thisGreen :: PixelComponent) <- peekByteOff ptr (offset + 1)
                (thisBlue  :: PixelComponent) <- peekByteOff ptr (offset + 2)
                return . (red =: thisRed) . (green =: thisGreen) . (blue =: thisBlue) . toPixelRGB $ leastIntensity
            else do
                (thisRed   :: PixelComponent) <- peekByteOff ptr offset
                (thisGreen :: PixelComponent) <- peekByteOff ptr (offset + 1)
                (thisBlue  :: PixelComponent) <- peekByteOff ptr (offset + 2)
                (thisAlpha :: PixelComponent) <- peekByteOff ptr (offset + 3)
                return . (red =: thisRed) . (green =: thisGreen) . (blue =: thisBlue) . (alpha =: thisAlpha) . toPixelRGB $ leastIntensity
    constructPixels f dms@(w, _) = unsafePerformIO $ do
        let isAlpha = case f (0, 0) of
                (PixelRGB  _) -> False
                (PixelBGR  _) -> False
                (PixelRGBA _) -> True
                (PixelBGRA _) -> True
            bytesPixel = if isAlpha then 4 else 3
        fbBitmap <- FB.newBitmap dms bytesPixel (Just 4)
        FB.withBitmap fbBitmap $ \(width, height) _ padding ptr -> do
            let bytesRow  = bytesPixel * w + padding
                maxRow    = abs . pred $ height
                maxColumn = abs . pred $ width
                m i@(row, column) = do
                    let offset = bytesRow * row + bytesPixel * column
                        pixel  = f i
                    pokeByteOff ptr offset           $ red   <: pixel
                    pokeByteOff ptr (offset + 1)     $ green <: pixel
                    pokeByteOff ptr (offset + 2)     $ blue  <: pixel
                    when (isAlpha) $ do
                        pokeByteOff ptr (offset + 3) $ alpha <: pixel
                    if column == maxColumn
                        then do
                            if row == maxRow
                                then do
                                    return ()
                                else do
                                    m (succ row, 0)
                        else do
                            m (row, succ column)
            m (0, 0)
        return $ BitmapForeign fbBitmap

instance BitmapSearchable  BitmapForeign
instance BitmapReflectable BitmapForeign
