{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, DeriveDataTypeable #-}

-- | Support for pixels with a colour depth of 24 or 32

module Data.Bitmap.Pixel
    ( Pixel(..)
    , PixelStorage
    , PixelComponent
    , leastIntensityComponent
    , greatestIntensityComponent
    , PixelRGB(..)
    , PixelBGR(..)
    , PixelRGBA(..)
    , PixelBGRA(..)
    , ConvPixelRGB(..)
    , ConvPixelBGR(..)
    , ConvPixelRGBA(..)
    , ConvPixelBGRA(..)
    ) where

import Control.Monad.Record
import Data.Bits
import Data.Data
import Data.Maybe
import Data.Word

class (Integral a) => Pixel a where
    red   ::        a :-> PixelComponent
    green ::        a :-> PixelComponent
    blue  ::        a :-> PixelComponent
    alpha :: Maybe (a :-> PixelComponent)
    leastIntensity    :: a
    greatestIntensity :: a

    leastIntensity = case alpha of
        (Just alpha') -> (red    =: leastIntensityComponent)
                       . (green  =: leastIntensityComponent)
                       . (blue   =: leastIntensityComponent)
                       . (alpha' =: leastIntensityComponent)
                       $ fromIntegral 0
        (Nothing)     -> (red    =: leastIntensityComponent)
                       . (green  =: leastIntensityComponent)
                       . (blue   =: leastIntensityComponent)
                       $ fromIntegral 0
    greatestIntensity = case alpha of
        (Just alpha') -> (red    =: greatestIntensityComponent)
                       . (green  =: greatestIntensityComponent)
                       . (blue   =: greatestIntensityComponent)
                       . (alpha' =: greatestIntensityComponent)
                       $ leastIntensity
        (Nothing)     -> (red    =: greatestIntensityComponent)
                       . (green  =: greatestIntensityComponent)
                       . (blue   =: greatestIntensityComponent)
                       $ leastIntensity

type PixelStorage   = Word32
type PixelComponent = Word8

leastIntensityComponent    :: PixelComponent
leastIntensityComponent    = 0x00
greatestIntensityComponent :: PixelComponent
greatestIntensityComponent = 0xFF

newtype PixelRGB  = PixelRGB  {unwrapPixelRGB  :: PixelStorage}
    deriving (Eq, Bounded, Enum, Ord, Real, Integral, Bits, Num, Show, Data, Typeable)
newtype PixelBGR  = PixelBGR  {unwrapPixelBGR  :: PixelStorage}
    deriving (Eq, Bounded, Enum, Ord, Real, Integral, Bits, Num, Show, Data, Typeable)
newtype PixelRGBA = PixelRGBA {unwrapPixelRGBA :: PixelStorage}
    deriving (Eq, Bounded, Enum, Ord, Real, Integral, Bits, Num, Show, Data, Typeable)
newtype PixelBGRA = PixelBGRA {unwrapPixelBGRA :: PixelStorage}
    deriving (Eq, Bounded, Enum, Ord, Real, Integral, Bits, Num, Show, Data, Typeable)

byteLens :: (Integral p, Bits p) => Integer -> (p :-> Word8)
byteLens 0 = lens (fromIntegral) (\w p -> p .|. fromIntegral w)
byteLens i = let i' = fromIntegral i
             in  lens (fromIntegral . (`shiftR` i')) (\w p -> p .|. fromIntegral w `shiftL` i')

instance Pixel PixelRGB where
    red   = byteLens 16
    green = byteLens 8
    blue  = byteLens 0
    alpha = Nothing

instance Pixel PixelBGR where
    red   = byteLens 0
    green = byteLens 8
    blue  = byteLens 16
    alpha = Nothing

instance Pixel PixelRGBA where
    red   = byteLens 8
    green = byteLens 16
    blue  = byteLens 24
    alpha = Just $ byteLens 0

instance Pixel PixelBGRA where
    red   = byteLens 24
    green = byteLens 16
    blue  = byteLens 8
    alpha = Just $ byteLens 0

class ConvPixelRGB  p where
    toPixelRGB    :: p -> PixelRGB
    fromPixelRGB  :: PixelRGB  -> p

class ConvPixelBGR  p where
    toPixelBGR    :: p -> PixelBGR
    fromPixelBGR  :: PixelBGR  -> p

class ConvPixelRGBA p where
    toPixelRGBA   :: p -> PixelRGBA
    fromPixelRGBA :: PixelRGBA -> p

class ConvPixelBGRA p where
    toPixelBGRA   :: p -> PixelBGRA
    fromPixelBGRA :: PixelBGRA -> p


instance ConvPixelRGB PixelRGB where
    toPixelRGB      = id
    fromPixelRGB    = id

instance ConvPixelRGB PixelBGR where
    toPixelRGB    b = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) $ leastIntensity
    fromPixelRGB    = toPixelBGR

instance ConvPixelRGB PixelRGBA where
    toPixelRGB    b = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) $ leastIntensity
    fromPixelRGB    = toPixelRGBA

instance ConvPixelRGB PixelBGRA where
    toPixelRGB    b = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) $ leastIntensity
    fromPixelRGB    = toPixelBGRA


instance ConvPixelBGR PixelRGB where
    toPixelBGR    b = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) $ leastIntensity
    fromPixelBGR    = toPixelRGB

instance ConvPixelBGR PixelBGR where
    toPixelBGR      = id
    fromPixelBGR    = id

instance ConvPixelBGR PixelRGBA where
    toPixelBGR    b = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) $ leastIntensity
    fromPixelBGR    = toPixelRGBA

instance ConvPixelBGR PixelBGRA where
    toPixelBGR    b = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) $ leastIntensity
    fromPixelBGR    = toPixelBGRA


instance ConvPixelRGBA PixelRGB where
    toPixelRGBA   b = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (fromJust alpha =: greatestIntensityComponent) $ leastIntensity
    fromPixelRGBA   = toPixelRGB

instance ConvPixelRGBA PixelBGR where
    toPixelRGBA   b = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (fromJust alpha =: greatestIntensityComponent) $ leastIntensity
    fromPixelRGBA   = toPixelBGR

instance ConvPixelRGBA PixelRGBA where
    toPixelRGBA     = id
    fromPixelRGBA   = id

instance ConvPixelRGBA PixelBGRA where
    toPixelRGBA   b = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (fromJust alpha =: fromJust alpha <: b) $ leastIntensity
    fromPixelRGBA   = toPixelBGRA


instance ConvPixelBGRA PixelRGB where
    toPixelBGRA   b = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (fromJust alpha =: greatestIntensityComponent) $ leastIntensity
    fromPixelBGRA   = toPixelRGB

instance ConvPixelBGRA PixelBGR where
    toPixelBGRA   b = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (fromJust alpha =: greatestIntensityComponent) $ leastIntensity
    fromPixelBGRA   = toPixelBGR

instance ConvPixelBGRA PixelRGBA where
    toPixelBGRA   b = (red =: red <: b) . (green =: green <: b) . (blue =: blue <: b) . (fromJust alpha =: fromJust alpha <: b) $ leastIntensity
    fromPixelBGRA   = toPixelRGBA

instance ConvPixelBGRA PixelBGRA where
    toPixelBGRA     = id
    fromPixelBGRA   = id
