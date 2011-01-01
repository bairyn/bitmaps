{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Data.Bitmap.Function.Internal
    ( BitmapFunction(..), bmpf_dimensions, bmpf_getPixel
    ) where

import Control.Monad.Record
import Data.Bitmap.Class
import Data.Bitmap.Pixel
import Data.Bitmap.Reflectable
import Data.Bitmap.Searchable
import Data.Bitmap.Types

data BitmapFunction = BitmapFunction
    { _bmpf_dimensions :: Dimensions Integer
    , _bmpf_getPixel   :: Coordinates Integer -> Pixel
    }

mkLabels [''BitmapFunction]

instance Bitmap BitmapFunction where
    type BIndexType BitmapFunction = Integer

    depth b = case (bmpf_getPixel <: b) (0, 0) of
        (PixelRGB  _) -> Depth24RGB
        (PixelBGR  _) -> Depth24RGB
        (PixelRGBA _) -> Depth32RGBA
        (PixelBGRA _) -> Depth32RGBA

    dimensions      = (bmpf_dimensions <:)

    getPixel        = (bmpf_getPixel   <:)

    constructPixels = flip BitmapFunction

instance BitmapSearchable  BitmapFunction
instance BitmapReflectable BitmapFunction
