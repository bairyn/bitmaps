{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Data.Bitmap.Function.Internal
    ( BitmapFunction(..), bmpf_dimensions, bmpf_getPixel
    ) where

import Control.Monad.Record
import Data.Bitmap.Class
import Data.Bitmap.Pixel
import Data.Bitmap.Types

data BitmapFunction = BitmapFunction
    { _bmpf_dimensions :: Dimensions Integer
    , _bmpf_getPixel   :: Coordinates Integer -> PixelRGBA
    }

mkLabels [''BitmapFunction]

instance Bitmap BitmapFunction where
    type BIndexType BitmapFunction = Integer
    type BPixelType BitmapFunction = PixelRGBA

    depth = const Depth32RGBA

    dimensions      = (bmpf_dimensions <:)

    getPixel        = (bmpf_getPixel   <:)

    constructPixels = flip BitmapFunction
