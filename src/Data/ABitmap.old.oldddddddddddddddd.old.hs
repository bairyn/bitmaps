{-# LANGUAGE PatternGuards, TypeOperators, ScopedTypeVariables #-}

module Data.ABitmap
    ( Bitmap
    , ForeignBitmap
    , PixelBGRA
    , BitmapRGB
    , PixelRGB

    -- * Conversion
    , toBitmap
    , toForeignBitmap

    -- * RGB Bitmaps
    , bitmapRGBToBGRA
    , bitmapBGRAToRGB

    -- * FBBitmap handling
    , unsafeWithBitmap
    , withForeignBitmap
    , fbBitmapToForeignBitmap
    , foreignBitmapToFBBitmap

    -- * Representing bitmaps as strings
    , persistBitmap64
    , encodeBitmapRGB24VerRef
    , encodeBitmapBGR24
    , encodeBitmapRGBA32
    , putBitmap
    , writeBitmap
    , decodeBitmap
    , unpersistBitmap64

    -- * Other bitmap formats
    , BitmapFormat(..)
    , unpersistBitmap

    -- * Bitmaps
    , mapBitmap
    , bitmapToPixelArray
    , pixelArrayToBitmap
    , bitmapDimensions

    -- * Pixels
    , pxR
    , pxG
    , pxB
    , pxA

    -- * 24-bit pixels
    , px24R
    , px24G
    , px24B
    ) where

import Prelude hiding ((.), id, (++))
import qualified Data.ByteString as B
import Codec.Compression.Zlib
import Codec.Image.STB hiding (Bitmap)
import Codec.String.Base16
import Codec.String.Base64
import Control.Applicative
import Control.Monad.Record
import Control.Spoon
import Data.ABitmap.Internal
import Data.Array.IArray
import qualified Data.Bitmap.Base as FB
import qualified Data.Bitmap.IO as FB
import Data.Bits
import Data.Foldable
import Data.Function
import Data.Int
import Data.List hiding ((++), foldl', foldl, foldr)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(mappend))
import Data.Serialize (encode)
import Data.Serialize.Builder
import qualified Data.String.Class as S
import Data.Word
import qualified Foreign (unsafePerformIO)
import Foreign.Ptr
import Foreign.Storable
import System.IO
import Text.Printf

-- | Converts a 'ForeignBitmap' to a 'Bitmap'
--
-- Since ForeignBitmap is really a reference, analogously to an IORef, this needs to be in the IO monad.
toBitmap :: ForeignBitmap -> IO Bitmap
toBitmap (ForeignBitmap fb) = FB.withBitmap fb f
    where f (w, h) channels padding (ptr :: Ptr Word8)
              | channels == 3 || channels == 4 =
                    let w', h', channels', padding' :: Integer
                        w'        = fromIntegral w
                        h'        = fromIntegral h
                        channels' = fromIntegral channels
                        padding'  = fromIntegral padding
                        isAlpha   = channels == 4
                    in  ((return . Bitmap . array ((0, 0), (abs . pred $ h', abs . pred $ w'))) =<<) . sequence $ do
                            row    <- [0 .. abs . pred $ h']
                            column <- [0 .. abs . pred $ w']
                            let base = channels' * (row * w' + column) + row * padding'
                                getByte = peekByteOff ptr . fromIntegral
                            return $
                                if isAlpha
                                    then do
                                        r <- getByte (base + 3)
                                        g <- getByte (base + 2)
                                        b <- getByte (base + 1)
                                        a <- getByte (base + 0)
                                        return $ ((row, column), (pxR =: r) . (pxG =: g) . (pxB =: b) . (pxA =: a) $ 0x00000000)
                                    else do
                                        r <- getByte (base + 2)
                                        g <- getByte (base + 1)
                                        b <- getByte (base + 0)
                                        return $ ((row, column), (pxR =: r) . (pxG =: g) . (pxB =: b) . (pxA =: 0xFF) $ 0x00000000)
              | otherwise = error $ printf "toBitmap: foreign bitmap with %d channels is unsupported; only bitmaps with 3 and 4 channels are supported" channels

        {-
              | 4 <- c
              , 0 <- p
                = do
                    fp <- newForeignPtr_ ptr
                    sa <- unsafeForeignPtrToStorableArray fp ((0, 0), (abs . pred . fromIntegral $ w, abs . pred . fromIntegral $ h))
                    a  <- freeze sa
                    return $ Bitmap a
              | 3 <- c
              , 0 <- p
                = do
                    fp <- newForeignPtr_ ptr
                    sa <- unsafeForeignPtrToStorableArray fp ((0, 0), (abs . pred . fromIntegral $ w, abs . pred . fromIntegral $ h))
                    a  <- freeze sa
                    let bitmapRGB = BitmapRGB a
                    return $ bitmapRGBToBGRA bitmapRGB
              | otherwise = error $ printf "toBitmap: bitmap with nchannels of '%d' and padding of '%d' is not supported"
              -}

--withFBitmap fb $ \(w, h) nch p ptr -> let w' = fromIntegral w; h' = fromIntegral h; w', h' :: Integer in do{a <- (sequence [do{ let{b = (fromIntegral nch) * (r_ * (w' + (fromIntegral p)) + c); f = peekElemOff ptr . fromIntegral}; r <- f (b+0); g <- f (b+1); b <- f (b+2); return $ ((r_, c), (pxR =: r) . (pxG =: g) . (pxB =: b) . (pxA =: 0xFF) $ 0x00000000) } | r_ <- [(0 :: Integer) .. abs . pred $ h'], c <- [(0 :: Integer) .. abs . pred $ w']]); return . Bitmap $ array ((0, 0), (abs . pred . fromIntegral $ w, abs . pred . fromIntegral $ h)) a}

-- | Converts a 'Bitmap' to the 'ForeignBitmap' reference
--
-- As internal memory must be allocated and used, this function returns an IO action.
toForeignBitmap :: Bitmap -> IO ForeignBitmap
toForeignBitmap = undefined{-
toForeignBitmap (Bitmap a) = do
    sa <- thaw a
    withStorableArray sa $ \p ->
        ForeignBitmap <$> flip (FB.copyBitmapFromPtr dim 4 0) (Just 1) p -- <* touchStorableArray sa
    where dim = let ((z0, z1), (a_, b)) = bounds a
                in  assert (z0 == 0 && z1 == 0) $ (fromIntegral a_, fromIntegral b)
                -}

bitmapRGBToBGRA :: BitmapRGB -> Bitmap
bitmapRGBToBGRA (BitmapRGB a) = Bitmap $ c <$> a
    where c px24 = ((pxR =: px24R <: px24) . (pxG =: px24G <: px24) . (pxB =: px24B <: px24) .(pxA =: 0xFF) $ 0x00000000)

bitmapBGRAToRGB :: Bitmap -> BitmapRGB
bitmapBGRAToRGB (Bitmap a) = BitmapRGB $ c <$> a
    where c px = ((px24R =: pxR <: px) . (px24G =: pxG <: px) . (px24B =: pxB <: px) $ 0x00000000)

-- | If the internal mutable memory isn't used after the function is used, this can be used to purely use a 'Bitmap' with a function that expects an 'FBBitmap'
--
-- Perhaps most importantly, any structure that involves the allocated memory, such as the FBBitmap itself, should not be returned.
-- If this function is used for read-only purposes (besides returning the FBBitmap, for example passing
-- "id", which would violate referential transparency and should not be done), the usage is probably safe.
unsafeWithBitmap :: (FB.Bitmap Word8 -> a) -> Bitmap -> a
unsafeWithBitmap f = withForeignBitmap f . Foreign.unsafePerformIO . toForeignBitmap

-- | This can be used for a function that expects an FBBitmap
withForeignBitmap :: (FB.Bitmap Word8 -> a) -> ForeignBitmap -> a
withForeignBitmap f = f . foreignBitmapToFBBitmap

-- | Converts an 'FB.Bitmap' to a 'ForeignBitmap'
fbBitmapToForeignBitmap :: FB.Bitmap Word8 -> ForeignBitmap
fbBitmapToForeignBitmap = ForeignBitmap

-- | Converts a 'ForeignBitmap' to an 'FB.Bitmap'
foreignBitmapToFBBitmap :: ForeignBitmap -> FB.Bitmap Word8
foreignBitmapToFBBitmap = unwrapForeignBitmap

-- | Encode a bitmap in bytes in the 24-bit BGR format ('BFBGR24Z64'); the alpha component and size information is lost
--
-- Before zlib compression, the size of the bytestring is calculated by "4 * numPixels" (the simplification of "ceil (4/3 * 3 * numPixels)").
-- Remember that the width and height of the bitmap are the *successor* to, respectively, the maximum column and maximum row, which are returned, in reverse order, from this function.
persistBitmap64 :: forall s. (S.StringConstruct s, S.StringLength s, S.StringEmpty s, S.StringLazyByteString s) => Bitmap -> (Integer, Integer, s)
persistBitmap64 b =
    let (r, c, s) = persistBitmap64' b
    in  (r, c, (S.toMainChar key $ 'm') `S.cons` s)
    where persistBitmap64' (Bitmap a) =
              let raw         = foldr' step empty_ a
                  step x acc  = (S.toMainChar key $ pxB <: x) `S.cons` (S.toMainChar key $ pxG <: x) `S.cons` (S.toMainChar key $ pxR <: x) `S.cons` acc
                  compressed  = S.fromLazyByteString . compress . S.toLazyByteString $ raw
                  encoded     = encode64 compressed
                  (_, (r, c)) = bounds a
              in  (succ c, succ r, encoded)
          key    = S.keyStringConstruct :: s
          empty_ = S.empty :: s

-- | Encode the pixels of bitmap in a ByteString in the RGB24 format, packed by rows appropriately for bitmaps, vertically reflected from the bottom left
--
-- Note: this is only a string of the raw pixel data
encodeBitmapRGB24VerRef :: forall s. (S.StringConstruct s, S.StringEmpty s) => Bitmap -> (Integer, Integer, s)
encodeBitmapRGB24VerRef (Bitmap a) =
    let (_, (maxRow, maxColumn)) = bounds a
        raw i@(r, c)
            | c == maxColumn =
                if r == 0
                    then prepPack . prep $ empty_
                    else prepPack . prep $ raw (pred r, 0)
            | otherwise = prep $ raw (r, succ c)
            where pixel  = a ! i
                  prep = \s -> (S.toMainChar key $ pxR <: pixel) `S.cons` (S.toMainChar key $ pxG <: pixel) `S.cons` (S.toMainChar key $ pxB <: pixel) `S.cons` s
                  prepPack
                      | (3 * succ maxColumn `mod` 4) == 0 = \s -> s
                      | (3 * succ maxColumn `mod` 4) == 1 = \s -> (S.toMainChar key padByte) `S.cons` (S.toMainChar key padByte) `S.cons` (S.toMainChar key padByte) `S.cons` s
                      | (3 * succ maxColumn `mod` 4) == 2 = \s -> (S.toMainChar key padByte) `S.cons` (S.toMainChar key padByte) `S.cons` s
                      | (3 * succ maxColumn `mod` 4) == 3 = \s -> (S.toMainChar key padByte) `S.cons` s
                      | otherwise              = \s -> s
                  padByte :: Word8
                  padByte = 0x00
    in  (succ maxColumn, succ maxRow, raw (maxRow, 0))
    where key    = S.keyStringConstruct :: s
          empty_ = S.empty :: s

-- | Encode the pixels of bitmap in a ByteString in the BGR24 format
encodeBitmapBGR24 :: forall s. (S.StringConstruct s, S.StringEmpty s) => Bitmap -> (Integer, Integer, s)
encodeBitmapBGR24 (Bitmap a) =
    let raw         = foldr' step empty_ a
        step x acc  = (S.toMainChar key $ pxB <: x) `S.cons` (S.toMainChar key $ pxG <: x) `S.cons` (S.toMainChar key $ pxR <: x) `S.cons` acc
        (_, (r, c)) = bounds a
    in  (succ c, succ r, raw)
    where key    = S.keyStringConstruct :: s
          empty_ = S.empty :: s

-- | Encode the pixels of bitmap in a ByteString in the RGBA32 format
encodeBitmapRGBA32 :: forall s. (S.StringConstruct s, S.StringEmpty s) => Bitmap -> (Integer, Integer, s)
encodeBitmapRGBA32 (Bitmap a) =
    let raw         = foldr' step empty_ a
        step x acc  = (S.toMainChar key $ pxR <: x) `S.cons` (S.toMainChar key $ pxG <: x) `S.cons` (S.toMainChar key $ pxB <: x) `S.cons` (S.toMainChar key $ pxA <: x) `S.cons` acc
        (_, (r, c)) = bounds a
    in  (succ c, succ r, raw)
    where key    = S.keyStringConstruct :: s
          empty_ = S.empty :: s

-- | Write the bitmap in the "bmp" format
--
-- The alpha component is lost.
--
-- TODO actually support *reading* this format; encode as a ByteString outside IO monad
putBitmap :: Handle -> Bitmap -> IO ()
putBitmap handle_ b = do
    let (w, h, raw) = encodeBitmapRGB24VerRef b
    -- TODO lazy bytestring?

    -- Magic sequence
    B.hPut handle_ $ encode (0x42 :: Word8)
    B.hPut handle_ $ encode (0x4D :: Word8)

    -- ERror reading bitmap header

    -- File size
    B.hPut handle_ $ encode (fromIntegral $ 3 * w * h + 0x0E + 40  :: Word32)

    -- Reserved
    B.hPut handle_ . toByteString . putWord16le $ (0x00 :: Word16)
    B.hPut handle_ . toByteString . putWord16le $ (0x00 :: Word16)

    -- Offset
    B.hPut handle_ . toByteString . putWord32le $ (0x0E + 40 :: Word32)

    -- Bitmap information header; BITMAPINFOHEADER
    -- header size
    B.hPut handle_ . toByteString . putWord32le $ (40 :: Word32)
    -- width
    B.hPut handle_ . toByteString . putWord32le . (fromIntegral :: Int32 -> Word32) $ (fromIntegral $ w  :: Int32)
    -- height
    B.hPut handle_ . toByteString . putWord32le . (fromIntegral :: Int32 -> Word32) $ (fromIntegral $ h  :: Int32)
    -- number of color planes
    B.hPut handle_ . toByteString . putWord16le $ (1 :: Word16)
    -- bits per pixel / depth
    B.hPut handle_ . toByteString . putWord16le $ (24 :: Word16)
    -- compression
    B.hPut handle_ . toByteString . putWord32le $ (0 :: Word32)  -- no compression
    -- image size
    B.hPut handle_ . toByteString . putWord32le $ (fromIntegral $ 3 * w * h  :: Word32)
    -- horizontal resolution; pixel per meter
    B.hPut handle_ . toByteString . putWord32le . (fromIntegral :: Int32 -> Word32) $ (3000 :: Int32)
    -- vertical resolution; pixel per meter
    B.hPut handle_ . toByteString . putWord32le . (fromIntegral :: Int32 -> Word32) $ (3000 :: Int32)
    -- number of colors
    B.hPut handle_ . toByteString . putWord32le $ (0 :: Word32)
    -- number of important colors
    B.hPut handle_ . toByteString . putWord32le $ (0 :: Word32)

    -- image
    B.hPut handle_ $ raw

-- | Equivalent to 'putBitmap'
writeBitmap :: FilePath -> Bitmap -> IO ()
writeBitmap fp b = withBinaryFile fp (WriteMode) (flip putBitmap b)

-- | Decode a bitmap; the formats that stb-image recognizes can be used
decodeBitmap :: (S.StringStrictByteString s, S.StringString s2) => s -> Either s2 Bitmap
decodeBitmap s =
    case Foreign.unsafePerformIO $ decodeImage (S.toStrictByteString s) of
        (Left  e)     -> Left (S.fromString e)
        --(Right image) -> let fbBitmap = (Unsafe.Coerce.unsafeCoerce (image :: FB.Bitmap Word8)) :: FB.Bitmap PixelBGRA  -- This is safe, as foreign pointers to different types are represented equivalently internally
                         --in  Right $ Foreign.unsafePerformIO $ toBitmap (ForeignBitmap $ fbBitmap)
        (Right image) -> Right $ Foreign.unsafePerformIO $ toBitmap (ForeignBitmap $ image)

-- | Decode a bitmap in the 24-bit BGR format ('BFBGR24Z64'); the data must be valid
--
-- As the encoded string does not contain size information, it must be passed to this function.
-- If the given dimensions are too small for the bitmap, a single-row bitmap
-- is created.  If there is extra pixel data, it is truncated.  The string
-- also does not contain the alpha component; the alpha component
-- of every pixel of the bitmap is set to 0xFF, the highest intensity.
unpersistBitmap64 :: forall s. (S.StringConstruct s, S.StringEmpty s, S.StringLazyByteString s) => (Integer, Integer, s) -> Bitmap
unpersistBitmap64 (w__, h__, s__) =
    let ~(Just (_, s'__)) = S.uncons s__
    in  unpersistBitmap64' (w__, h__, s'__)
    where unpersistBitmap64' (w, h, s) =
              let decoded   = decode64 s
                  raw :: s
                  raw       = S.fromLazyByteString . decompress . S.toLazyByteString $ decoded
                  p         = fix f raw
                  f c s_
                      | (Just (b, g, r, s')) <- S.uncons3 s_ =
                            let (red, green, blue) = (S.toWord8 r, S.toWord8 g, S.toWord8 b)
                            in  ((pxR =: red) . (pxG =: green) . (pxB =: blue) . (pxA =: 0xFF) $ 0x00000000) : c s'
                      | otherwise                              =
                            []
                  numPixels = genericLength p
                  fits      = (abs w) * (abs h) >= numPixels
                  w'
                      | fits      = w
                      | otherwise = numPixels
                  h'
                      | fits      = h
                      | otherwise = 1
              in  Bitmap . listArray ((0, 0), (pred h', pred w')) $ reverse p

data BitmapFormat =
    BFBGR24Z64
  | BFSinglePixel
  | BFRGB24EGBRZ64
  | BFHZHRGB24
  | BFHRGB24
    deriving (Eq, Ord, Show, Read, Enum)

-- | Decode a bitmap; the formats in 'BitmapFormat' are supported

-- Implementation note: this code is fragile; edit with care.  Pay particular attention to when laziness is required.
-- A more flexible implementation would be a list of possible valid interpretations; it would clean this
-- up a lot.
unpersistBitmap :: forall s. (S.StringConstruct s, S.StringLength s, S.StringPack s, S.StringEmpty s, S.StringLazyByteString s) => (Integer, Integer, s) -> Maybe (BitmapFormat, Bitmap)
unpersistBitmap t__@(_, _, s__) = let ~(Just (first__, tail__)) = S.uncons s__
                                  in  unpersistBitmap' (S.length s__) (S.toChar first__) (S.fromLazyByteString <$> (teaspoon . decompress . S.toLazyByteString . decode64 $ tail__)  :: Maybe s) t__
    where unpersistBitmap' len first canUncompress (w, h, s)
              | len == 0
                -- This code assumes that there is at least one byte
                  = Nothing
              | len == 6
              -- We use different accessors to access the correct color bytes of the byte
              , (Just (rgb :: Word32)) <- maybeRead $ "0x00" ++ S.unpack s
                  = Just (BFSinglePixel, Bitmap $ array ((0, 0), (0, 0)) [((0, 0), (pxR =: pxG <: rgb) . (pxG =: pxR <: rgb) . (pxB =: pxA <: rgb). (pxA =: 0xFF) $ 0x00000000)])
              | (first == 'm' || first == 'b') -- && isJust canUncompress
                  = case () of
                             _ | (Just raw) <- canUncompress
                                  -> case () of
                                            _ | first == 'm'
                                                 -> Just (BFBGR24Z64, unpersistBitmap64 (w, h, s))
                                              | first == 'b'
                                                 -> let p = rgb raw
                                                        rgb s_
                                                            | (Just (a_, b_, c_, s')) <- S.uncons3 s_ =
                                                                  case () of
                                                                         _ | (Just (True)) <- S.null <$> (S.tail =<< S.tail =<< S.tail s')
                                                                              -> let (g2, b2, red2) = (S.toWord8 a_, S.toWord8 b_, S.toWord8 c_)
                                                                                     (red, g, b) = (S.toWord8 a_, S.toWord8 b_, S.toWord8 c_)
                                                                                 in  ((pxR =: red) . (pxG =: g) . (pxB =: b) . (pxA =: 0xFF) $ 0x00000000)
                                                                                   : [((pxR =: red2) . (pxG =: g2) . (pxB =: b2) . (pxA =: 0xFF) $ 0x00000000)]
                                                                           | otherwise
                                                                              -> let (red, g, b) = (S.toWord8 a_, S.toWord8 b_, S.toWord8 c_)
                                                                                 in  ((pxR =: red) . (pxG =: g) . (pxB =: b) . (pxA =: 0xFF) $ 0x00000000) : rgb s'
                                                            | otherwise = []
                                                    in  case () of
                                                               _ | (abs w) * (abs h) >= genericLength p
                                                                    -> Just (BFRGB24EGBRZ64, Bitmap . listArray ((0, 0), (abs . pred $ h, abs . pred $ w)) $ reverse p)
                                                                 | otherwise
                                                                    -> Just (BFRGB24EGBRZ64, Bitmap . listArray ((0, 0), (0, abs . pred $ genericLength p)) $ reverse p)
                                              | otherwise -> Nothing
                               | otherwise -> Nothing
              | first == 'z'
                  = let ~(Just tail_) = S.tail s
                        compressed    = decodeHex tail_
                    in  case () of
                               _ | (Just raw) <- (S.fromLazyByteString <$> (teaspoon . decompress . S.toLazyByteString $ compressed)  :: Maybe s)
                                    -> let decoded = decodeHex raw
                                           p = rgb decoded
                                           rgb s_
                                               | (Just (a_, b_, c_, s')) <- S.uncons3 s_ =
                                                     let (red, g, b) = (S.toWord8 a_, S.toWord8 b_, S.toWord8 c_)
                                                     in  ((pxR =: red) . (pxG =: g) . (pxB =: b) . (pxA =: 0xFF) $ 0x00000000) : rgb s'
                                               | otherwise = []
                                       in  case () of
                                                  _ | (abs w) * (abs h) >= genericLength p
                                                       -> Just (BFHZHRGB24, Bitmap . listArray ((0, 0), (abs . pred $ h, abs . pred $ w)) $ reverse p)
                                                    | otherwise
                                                       -> Just (BFHZHRGB24, Bitmap . listArray ((0, 0), (0, abs . pred $ genericLength p)) $ reverse p)
                                 | otherwise
                                    -> Nothing
              | len == 6 * w * h
                  = let decoded = decodeHex s
                        p = rgb decoded
                        rgb s_
                            | (Just (a_, b_, c_, s')) <- S.uncons3 s_ =
                                  let (red, g, b) = (S.toWord8 a_, S.toWord8 b_, S.toWord8 c_)
                                  in  ((pxR =: red) . (pxG =: g) . (pxB =: b) . (pxA =: 0xFF) $ 0x00000000) : rgb s'
                            | otherwise = []
                    in  case () of
                               _ | (abs w) * (abs h) >= genericLength p
                                    -> Just (BFHRGB24, Bitmap . listArray ((0, 0), (abs . pred $ h, abs . pred $ w)) $ reverse p)
                                 | otherwise
                                    -> Just (BFHRGB24, Bitmap . listArray ((0, 0), (0, abs . pred $ genericLength p)) $ reverse p)
              | otherwise
                  = Nothing

-- | Map a bitmap, pixel by pixel
mapBitmap :: (PixelBGRA -> PixelBGRA) -> Bitmap -> Bitmap
mapBitmap f = Bitmap . fmap f . unwrapBitmap

-- | Return an array of pixels from the bitmap
bitmapToPixelArray :: Bitmap -> Array (Integer, Integer) PixelBGRA
bitmapToPixelArray = unwrapBitmap

-- | Construct a bitmap from an array of pixels
pixelArrayToBitmap :: Array (Integer, Integer) PixelBGRA -> Bitmap
pixelArrayToBitmap = Bitmap

-- | Returns the dimensions as a tuple of (width, height)
bitmapDimensions :: Bitmap -> (Integer, Integer)
bitmapDimensions (Bitmap a) =
    let (_, (mr, mc)) = bounds a
    in  (abs . succ $ mc, abs . succ $ mr)

byteLabel :: (Integral p, Bits p) => Integer -> (p :-> Word8)
byteLabel 0 = lens (fromIntegral) (\w p -> p .|. fromIntegral w)
byteLabel i = let i' = fromIntegral i
              in  lens (fromIntegral . flip shiftR i') (\w p -> p .|. flip shiftL i' (fromIntegral w))

-- | Access the 'R' byte of a pixel
pxR :: PixelBGRA :-> Word8
pxR = byteLabel 8

-- | Access the 'G' byte of a pixel
pxG :: PixelBGRA :-> Word8
pxG = byteLabel 16

-- | Access the 'B' byte of a pixel
pxB :: PixelBGRA :-> Word8
pxB = byteLabel 24

-- | Access the 'A' byte of a pixel
pxA :: PixelBGRA :-> Word8
pxA = byteLabel 0

-- | Access the 'R' byte of a 24-bit pixel
px24R :: PixelRGB :-> Word8
px24R = byteLabel 16

-- | Access the 'G' byte of a 24-bit pixel
px24G :: PixelRGB :-> Word8
px24G = byteLabel 8

-- | Access the 'B' byte of a 24-bit pixel
px24B :: PixelRGB :-> Word8
px24B = byteLabel 0

(++) :: (Monoid a) => a -> a -> a
(++) = mappend
infixr 5 ++

maybeRead :: (S.StringString s) => Read a => s -> Maybe a
maybeRead = (>>= \ ~(r, s') -> if S.null s' then Just r else Nothing) . listToMaybe . reads . S.toString
