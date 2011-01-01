{-# LANGUAGE TypeOperators, DeriveDataTypeable #-}

-- | Support for pixels with a colour depth of 24 or 32, either lacking or containing an alpha component

module Data.Bitmap.Pixel
    ( Pixel(..)
    , eqPixelValue, neqPixelValue
    , red, green, blue, alpha
    , toPixelRGB, toPixelBGR, toPixelRGBA, toPixelBGRA
    , PixelStorage
    , PixelComponent
    , leastIntensityComponent
    , greatestIntensityComponent
    , leastIntensity
    , greatestIntensity
    ) where

import Control.Monad.Record
import Data.Bits
import Data.Data
import Data.Word

data Pixel =
    PixelRGB  {unwrapPixelStorage :: PixelStorage}  -- ^ The most significant byte is unused
  | PixelBGR  {unwrapPixelStorage :: PixelStorage}  -- ^ The most significant byte is unused
  | PixelRGBA {unwrapPixelStorage :: PixelStorage}
  | PixelBGRA {unwrapPixelStorage :: PixelStorage}
    deriving (Eq, Show, Data, Typeable)

-- | If the pixel types differ, they can still be determined to be equivalent if their components are equal
--
-- Unlike the default derived instance of Eq, 
eqPixelValue, neqPixelValue :: Pixel -> Pixel -> Bool
a `eqPixelValue`  b = red <: a == red <: b && green <: a == green <: b && blue <: a == blue <: b
a `neqPixelValue` b = red <: a /= red <: b || green <: a /= green <: b || blue <: a /= blue <: b

lgetter :: Integer -> (PixelStorage -> PixelComponent)
lgetter 0 = fromIntegral
lgetter i = \storage -> (fromIntegral :: PixelStorage -> PixelComponent) $ storage `shiftR` fromIntegral i

lsetter :: Integer -> (PixelComponent -> PixelStorage -> PixelStorage)
lsetter 0 = \component storage -> storage .|. fromIntegral component
lsetter i = \component storage -> storage .|. (((fromIntegral (component :: PixelComponent)) :: PixelStorage) `shiftL` (fromIntegral i))

red :: Pixel :-> PixelComponent
red = lens getter setter
    where getter           (PixelRGB  storage) =             lgetter 16 storage
          getter           (PixelBGR  storage) =             lgetter 0  storage
          getter           (PixelRGBA storage) =             lgetter 24 storage
          getter           (PixelBGRA storage) =             lgetter 8  storage
          setter component (PixelRGB  storage) = PixelRGB  $ lsetter 16 component storage
          setter component (PixelBGR  storage) = PixelBGR  $ lsetter 0  component storage
          setter component (PixelRGBA storage) = PixelRGBA $ lsetter 24 component storage
          setter component (PixelBGRA storage) = PixelBGRA $ lsetter 8  component storage

green :: Pixel :-> PixelComponent
green = lens getter setter
    where getter           (PixelRGB  storage) =             lgetter 8  storage
          getter           (PixelBGR  storage) =             lgetter 8  storage
          getter           (PixelRGBA storage) =             lgetter 16 storage
          getter           (PixelBGRA storage) =             lgetter 16 storage
          setter component (PixelRGB  storage) = PixelRGB  $ lsetter 8  component storage
          setter component (PixelBGR  storage) = PixelBGR  $ lsetter 8  component storage
          setter component (PixelRGBA storage) = PixelRGBA $ lsetter 16 component storage
          setter component (PixelBGRA storage) = PixelBGRA $ lsetter 16 component storage

blue :: Pixel :-> PixelComponent
blue = lens getter setter
    where getter           (PixelRGB  storage) =             lgetter 0  storage
          getter           (PixelBGR  storage) =             lgetter 16 storage
          getter           (PixelRGBA storage) =             lgetter 8  storage
          getter           (PixelBGRA storage) =             lgetter 24 storage
          setter component (PixelRGB  storage) = PixelRGB  $ lsetter 0  component storage
          setter component (PixelBGR  storage) = PixelBGR  $ lsetter 16 component storage
          setter component (PixelRGBA storage) = PixelRGBA $ lsetter 8  component storage
          setter component (PixelBGRA storage) = PixelBGRA $ lsetter 24 component storage

alpha :: Pixel :-> PixelComponent
alpha = lens getter setter
    where getter             (PixelRGB  _)       =             greatestIntensityComponent
          getter             (PixelBGR  _)       =             greatestIntensityComponent
          getter             (PixelRGBA storage) =             lgetter 0  storage
          getter             (PixelBGRA storage) =             lgetter 0  storage
          setter _         b@(PixelRGB  _)       = b
          setter _         b@(PixelBGR  _)       = b
          setter component   (PixelRGBA storage) = PixelRGBA $ lsetter 0  component storage
          setter component   (PixelBGRA storage) = PixelBGRA $ lsetter 0  component storage

toPixelRGB  :: Pixel -> Pixel
toPixelRGB  b@(PixelRGB  _) = b
toPixelRGB  b@(PixelBGR  _) = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (alpha =: alpha <: b) $ PixelRGB  0
toPixelRGB  b@(PixelRGBA _) = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (alpha =: alpha <: b) $ PixelRGB  0
toPixelRGB  b@(PixelBGRA _) = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (alpha =: alpha <: b) $ PixelRGB  0

toPixelBGR  :: Pixel -> Pixel
toPixelBGR  b@(PixelRGB  _) = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (alpha =: alpha <: b) $ PixelBGR  0
toPixelBGR  b@(PixelBGR  _) = b
toPixelBGR  b@(PixelRGBA _) = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (alpha =: alpha <: b) $ PixelBGR  0
toPixelBGR  b@(PixelBGRA _) = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (alpha =: alpha <: b) $ PixelBGR  0

toPixelRGBA :: Pixel -> Pixel
toPixelRGBA b@(PixelRGB  _) = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (alpha =: alpha <: b) $ PixelRGBA 0
toPixelRGBA b@(PixelBGR  _) = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (alpha =: alpha <: b) $ PixelRGBA 0
toPixelRGBA b@(PixelRGBA _) = b
toPixelRGBA b@(PixelBGRA _) = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (alpha =: alpha <: b) $ PixelRGBA 0

toPixelBGRA :: Pixel -> Pixel
toPixelBGRA b@(PixelRGB  _) = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (alpha =: alpha <: b) $ PixelBGRA 0
toPixelBGRA b@(PixelBGR  _) = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (alpha =: alpha <: b) $ PixelBGRA 0
toPixelBGRA b@(PixelRGBA _) = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (alpha =: alpha <: b) $ PixelBGRA 0
toPixelBGRA b@(PixelBGRA _) = b

type PixelStorage   = Word32
type PixelComponent = Word8

leastIntensityComponent    :: PixelComponent
leastIntensityComponent    = 0x00
greatestIntensityComponent :: PixelComponent
greatestIntensityComponent = 0xFF

leastIntensity :: Pixel
leastIntensity = (red   =: leastIntensityComponent)
               . (green =: leastIntensityComponent)
               . (blue  =: leastIntensityComponent)
               . (alpha =: leastIntensityComponent)
               $ PixelRGBA 0
greatestIntensity :: Pixel
greatestIntensity = (red   =: greatestIntensityComponent)
                  . (green =: greatestIntensityComponent)
                  . (blue  =: greatestIntensityComponent)
                  . (alpha =: greatestIntensityComponent)
                  $ PixelRGBA 0
