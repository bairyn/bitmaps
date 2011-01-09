{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, DeriveDataTypeable #-}

-- | Support for pixels with a colour depth of 24 or 32, either lacking or containing an alpha component

module Data.Bitmap.Pixel
    ( Pixel(..)
    , convertPixelValue
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

    , GenPixel(..)
    , eqGenPixelValue, neqGenPixelValue
    , genRed, genGreen, genBlue, genAlpha
    , toGenPixelRGB, toGenPixelBGR, toGenPixelRGBA, toGenPixelBGRA
    , GenPixelStorage
    , GenPixelComponent
    , leastIntensityGenComponent
    , greatestIntensityGenComponent
    , leastIntensityGen
    , greatestIntensityGen

    , bigEndian
    ) where

import Control.Applicative
import Control.Monad.Record
import Data.Bits
import Data.Data
import Data.Maybe
import Data.Word
import Foreign (unsafePerformIO)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable

class (Integral a, ConvPixelRGB a, ConvPixelRGBA a, ConvPixelBGR a, ConvPixelBGRA a) => Pixel a where
    red   ::        a :-> PixelComponent
    green ::        a :-> PixelComponent
    blue  ::        a :-> PixelComponent
    alpha :: Maybe (a :-> PixelComponent)
    leastIntensity    :: a
    greatestIntensity :: a
    toPixel   :: (Pixel p) => a -> p
    fromPixel :: (Pixel p) => p -> a

    leastIntensity = case alpha of
        (Just alpha') -> (red    =: leastIntensityComponent)
                       . (green  =: leastIntensityComponent)
                       . (blue   =: leastIntensityComponent)
                       . (alpha' =: leastIntensityComponent)
                       $ fromIntegral (0 :: Integer)
        (Nothing)     -> (red    =: leastIntensityComponent)
                       . (green  =: leastIntensityComponent)
                       . (blue   =: leastIntensityComponent)
                       $ fromIntegral (0 :: Integer)
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

    toPixel   = convertPixelValue
    fromPixel = convertPixelValue

-- | A less efficient way of converting pixels by their components
convertPixelValue :: (Pixel a, Pixel b) => a -> b
convertPixelValue p =
    case alpha of
        (Just alphaB) ->
            case alpha of
                (Just alphaA) ->
                    (red    =: red    <: p)
                  . (green  =: green  <: p)
                  . (blue   =: blue   <: p)
                  . (alphaB =: alphaA <: p)
                  $ leastIntensity
                (Nothing)     ->
                    (red    =: red    <: p)
                  . (green  =: green  <: p)
                  . (blue   =: blue   <: p)
                  . (alphaB =: greatestIntensityComponent)
                  $ leastIntensity
        (Nothing)     ->
                    (red    =: red    <: p)
                  . (green  =: green  <: p)
                  . (blue   =: blue   <: p)
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
    toPixel   = fromPixelRGB
    fromPixel = toPixelRGB

instance Pixel PixelBGR where
    red   = byteLens 0
    green = byteLens 8
    blue  = byteLens 16
    alpha = Nothing
    toPixel   = fromPixelBGR
    fromPixel = toPixelBGR

instance Pixel PixelRGBA where
    red   = byteLens 8
    green = byteLens 16
    blue  = byteLens 24
    alpha = Just $ byteLens 0
    toPixel   = fromPixelRGBA
    fromPixel = toPixelRGBA

instance Pixel PixelBGRA where
    red   = byteLens 24
    green = byteLens 16
    blue  = byteLens 8
    alpha = Just $ byteLens 0
    toPixel   = fromPixelBGRA
    fromPixel = toPixelBGRA

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

-- | Generic pixel type which has not be efficient enough when used with bitmaps in practice
data GenPixel =
    GenPixelRGB  {unwrapPixelStorage :: GenPixelStorage}  -- ^ The most significant byte is unused
  | GenPixelBGR  {unwrapPixelStorage :: GenPixelStorage}  -- ^ The most significant byte is unused
  | GenPixelRGBA {unwrapPixelStorage :: GenPixelStorage}
  | GenPixelBGRA {unwrapPixelStorage :: GenPixelStorage}
    deriving (Eq, Show, Data, Typeable)

-- | If the Genpixel types differ, they can still be determined to be equivalent if their components are equal
--
-- Unlike the default derived instance of Eq, 
eqGenPixelValue, neqGenPixelValue :: GenPixel -> GenPixel -> Bool
a `eqGenPixelValue`  b = genRed <: a == genRed <: b && genGreen <: a == genGreen <: b && genBlue <: a == genBlue <: b
a `neqGenPixelValue` b = genRed <: a /= genRed <: b || genGreen <: a /= genGreen <: b || genBlue <: a /= genBlue <: b

lgetter :: Integer -> (GenPixelStorage -> GenPixelComponent)
lgetter 0 = fromIntegral
lgetter i = \storage -> (fromIntegral :: GenPixelStorage -> GenPixelComponent) $ storage `shiftR` fromIntegral i

lsetter :: Integer -> (GenPixelComponent -> GenPixelStorage -> GenPixelStorage)
lsetter 0 = \component storage -> storage .|. fromIntegral component
lsetter i = \component storage -> storage .|. (((fromIntegral (component :: GenPixelComponent)) :: GenPixelStorage) `shiftL` (fromIntegral i))

genRed :: GenPixel :-> GenPixelComponent
genRed = lens getter setter
    where getter           (GenPixelRGB  storage) =             lgetter 16 storage
          getter           (GenPixelBGR  storage) =             lgetter 0  storage
          getter           (GenPixelRGBA storage) =             lgetter 24 storage
          getter           (GenPixelBGRA storage) =             lgetter 8  storage
          setter component (GenPixelRGB  storage) = GenPixelRGB  $ lsetter 16 component storage
          setter component (GenPixelBGR  storage) = GenPixelBGR  $ lsetter 0  component storage
          setter component (GenPixelRGBA storage) = GenPixelRGBA $ lsetter 24 component storage
          setter component (GenPixelBGRA storage) = GenPixelBGRA $ lsetter 8  component storage

genGreen :: GenPixel :-> GenPixelComponent
genGreen = lens getter setter
    where getter           (GenPixelRGB  storage) =             lgetter 8  storage
          getter           (GenPixelBGR  storage) =             lgetter 8  storage
          getter           (GenPixelRGBA storage) =             lgetter 16 storage
          getter           (GenPixelBGRA storage) =             lgetter 16 storage
          setter component (GenPixelRGB  storage) = GenPixelRGB  $ lsetter 8  component storage
          setter component (GenPixelBGR  storage) = GenPixelBGR  $ lsetter 8  component storage
          setter component (GenPixelRGBA storage) = GenPixelRGBA $ lsetter 16 component storage
          setter component (GenPixelBGRA storage) = GenPixelBGRA $ lsetter 16 component storage

genBlue :: GenPixel :-> GenPixelComponent
genBlue = lens getter setter
    where getter           (GenPixelRGB  storage) =             lgetter 0  storage
          getter           (GenPixelBGR  storage) =             lgetter 16 storage
          getter           (GenPixelRGBA storage) =             lgetter 8  storage
          getter           (GenPixelBGRA storage) =             lgetter 24 storage
          setter component (GenPixelRGB  storage) = GenPixelRGB  $ lsetter 0  component storage
          setter component (GenPixelBGR  storage) = GenPixelBGR  $ lsetter 16 component storage
          setter component (GenPixelRGBA storage) = GenPixelRGBA $ lsetter 8  component storage
          setter component (GenPixelBGRA storage) = GenPixelBGRA $ lsetter 24 component storage

genAlpha :: GenPixel :-> GenPixelComponent
genAlpha = lens getter setter
    where getter             (GenPixelRGB  _)       =             greatestIntensityComponent
          getter             (GenPixelBGR  _)       =             greatestIntensityComponent
          getter             (GenPixelRGBA storage) =             lgetter 0  storage
          getter             (GenPixelBGRA storage) =             lgetter 0  storage
          setter _         b@(GenPixelRGB  _)       = b
          setter _         b@(GenPixelBGR  _)       = b
          setter component   (GenPixelRGBA storage) = GenPixelRGBA $ lsetter 0  component storage
          setter component   (GenPixelBGRA storage) = GenPixelBGRA $ lsetter 0  component storage

toGenPixelRGB  :: GenPixel -> GenPixel
toGenPixelRGB  b@(GenPixelRGB  _) = b
toGenPixelRGB  b@(GenPixelBGR  _) = (genRed =: genRed <: b) . (genGreen =: genGreen <: b) . (genBlue =: genBlue <: b) . (genAlpha =: genAlpha <: b) $ GenPixelRGB  0
toGenPixelRGB  b@(GenPixelRGBA _) = (genRed =: genRed <: b) . (genGreen =: genGreen <: b) . (genBlue =: genBlue <: b) . (genAlpha =: genAlpha <: b) $ GenPixelRGB  0
toGenPixelRGB  b@(GenPixelBGRA _) = (genRed =: genRed <: b) . (genGreen =: genGreen <: b) . (genBlue =: genBlue <: b) . (genAlpha =: genAlpha <: b) $ GenPixelRGB  0

toGenPixelBGR  :: GenPixel -> GenPixel
toGenPixelBGR  b@(GenPixelRGB  _) = (genRed =: genRed <: b) . (genGreen =: genGreen <: b) . (genBlue =: genBlue <: b) . (genAlpha =: genAlpha <: b) $ GenPixelBGR  0
toGenPixelBGR  b@(GenPixelBGR  _) = b
toGenPixelBGR  b@(GenPixelRGBA _) = (genRed =: genRed <: b) . (genGreen =: genGreen <: b) . (genBlue =: genBlue <: b) . (genAlpha =: genAlpha <: b) $ GenPixelBGR  0
toGenPixelBGR  b@(GenPixelBGRA _) = (genRed =: genRed <: b) . (genGreen =: genGreen <: b) . (genBlue =: genBlue <: b) . (genAlpha =: genAlpha <: b) $ GenPixelBGR  0

toGenPixelRGBA :: GenPixel -> GenPixel
toGenPixelRGBA b@(GenPixelRGB  _) = (genRed =: genRed <: b) . (genGreen =: genGreen <: b) . (genBlue =: genBlue <: b) . (genAlpha =: genAlpha <: b) $ GenPixelRGBA 0
toGenPixelRGBA b@(GenPixelBGR  _) = (genRed =: genRed <: b) . (genGreen =: genGreen <: b) . (genBlue =: genBlue <: b) . (genAlpha =: genAlpha <: b) $ GenPixelRGBA 0
toGenPixelRGBA b@(GenPixelRGBA _) = b
toGenPixelRGBA b@(GenPixelBGRA _) = (genRed =: genRed <: b) . (genGreen =: genGreen <: b) . (genBlue =: genBlue <: b) . (genAlpha =: genAlpha <: b) $ GenPixelRGBA 0

toGenPixelBGRA :: GenPixel -> GenPixel
toGenPixelBGRA b@(GenPixelRGB  _) = (genRed =: genRed <: b) . (genGreen =: genGreen <: b) . (genBlue =: genBlue <: b) . (genAlpha =: genAlpha <: b) $ GenPixelBGRA 0
toGenPixelBGRA b@(GenPixelBGR  _) = (genRed =: genRed <: b) . (genGreen =: genGreen <: b) . (genBlue =: genBlue <: b) . (genAlpha =: genAlpha <: b) $ GenPixelBGRA 0
toGenPixelBGRA b@(GenPixelRGBA _) = (genRed =: genRed <: b) . (genGreen =: genGreen <: b) . (genBlue =: genBlue <: b) . (genAlpha =: genAlpha <: b) $ GenPixelBGRA 0
toGenPixelBGRA b@(GenPixelBGRA _) = b

type GenPixelStorage   = Word32
type GenPixelComponent = Word8

leastIntensityGenComponent    :: GenPixelComponent
leastIntensityGenComponent    = 0x00
greatestIntensityGenComponent :: GenPixelComponent
greatestIntensityGenComponent = 0xFF

leastIntensityGen :: GenPixel
leastIntensityGen = (genRed   =: leastIntensityGenComponent)
                  . (genGreen =: leastIntensityGenComponent)
                  . (genBlue  =: leastIntensityGenComponent)
                  . (genAlpha =: leastIntensityGenComponent)
                  $ GenPixelRGBA 0
greatestIntensityGen :: GenPixel
greatestIntensityGen = (genRed   =: greatestIntensityGenComponent)
                     . (genGreen =: greatestIntensityGenComponent)
                     . (genBlue  =: greatestIntensityGenComponent)
                     . (genAlpha =: greatestIntensityGenComponent)
                     $ GenPixelRGBA 0

bigEndian :: Bool
bigEndian = unsafePerformIO $ with (1 :: CInt) $ \p -> (0 ==) <$> (peek (castPtr p :: Ptr CChar))
