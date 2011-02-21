{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}

module Data.Bitmap.Searchable
    ( BitmapSearchable(..)
    , areColorsSimilar
    , colorDifferenceCIE94
    , defaultTransparentPixel
    , matchPixelAny
    , matchPixelSame
    , matchPixelDif
    , matchPixelDifThreshold
    ) where

import Control.Monad.Record
import Data.Bitmap.Class
import Data.Bitmap.Pixel
import Data.Bitmap.Types
import Data.List (nub)

-- | Class for searchable bitmaps
--
-- Using the functions of the 'Bitmap' class,
-- default functions are be defined for each of
-- these; of course, implementations are free
-- to define more efficient versions.
class (Bitmap bmp) => BitmapSearchable bmp where
    findPixel ::
        (BPixelType bmp -> Bool)
     -> bmp
     -> Maybe (Coordinates (BIndexType bmp))    -- ^ Scan each pixel until a match is found in no particular order
                                                --
                                                -- Implementations are free to choose an efficient implementation that
                                                -- searches in a different direction from that of 'findPixelOrder'.
                                                -- This function is often, but not necessarily always, the same as
                                                -- 'findPixelOrder'.
    findPixelOrder ::
        (BPixelType bmp -> Bool)
     -> bmp
     -> Coordinates (BIndexType bmp)
     -> Maybe (Coordinates (BIndexType bmp))    -- ^ Scan each pixel, row by row from the left, starting at the given offset, until a match is found
    findPixelEqual ::
        BPixelType bmp
     -> bmp
     -> Coordinates (BIndexType bmp)
     -> Maybe (Coordinates (BIndexType bmp))    -- ^ A more restricted version of 'findPixelEqual' that is usually more efficient when exact equality is desired
    findPixels ::
        (BPixelType bmp -> Bool)
     -> bmp
     -> Coordinates (BIndexType bmp)
     -> [Coordinates (BIndexType bmp)]
    findPixelsEqual ::
        BPixelType bmp
     -> bmp
     -> Coordinates (BIndexType bmp)
     -> [Coordinates (BIndexType bmp)]

    findSubBitmap ::
        (BPixelType bmp -> BPixelType bmp -> Bool)
     -> bmp  -- Super bitmap
     -> bmp  -- Sub bitmap
     -> Maybe (Coordinates (BIndexType bmp))    -- ^ Search for where a sub-bitmap would match
                                                --
                                                -- Each coordinate, representing the upper-left-most corner,
                                                -- for which the sub-bitmap would fit is tried for a match until
                                                -- the function returns 'True' for every pixel that is compared.
                                                -- The function is passed the pixel of the super bitmap which is searched
                                                -- as the first parameter, and the pixel of the sub bitmap is passed
                                                -- as the second parameter.  Likewise, the super bitmap is then given
                                                -- to this function as the second parameter, and then the sub bitmap.
                                                -- Normally, the order in which the bitmap is checked in the same order
                                                -- as 'findPixelOrder', but implementation are free to implement this
                                                -- in whatever order is convenient or efficient; implementation should,
                                                -- however, assume that callers usually expect this order to be the most
                                                -- efficient one.
    findSubBitmapOrder ::
        (BPixelType bmp -> BPixelType bmp -> Bool)
     -> bmp  -- Super bitmap
     -> bmp  -- Sub bitmap
     -> Coordinates (BIndexType bmp)
     -> Maybe (Coordinates (BIndexType bmp))  
    findSubBitmapEqual ::
        bmp  -- Super bitmap
     -> bmp  -- Sub bitmap
     -> Coordinates (BIndexType bmp)
     -> Maybe (Coordinates (BIndexType bmp))    -- ^ A more restricted version of 'findSubBitmapEqual' that is usually more efficient when exact equality is desired
    findSubBitmaps ::
        (BPixelType bmp -> BPixelType bmp -> Bool)
     -> bmp  -- Super bitmap
     -> bmp  -- Sub bitmap
     -> Coordinates (BIndexType bmp)
     -> [(Coordinates (BIndexType bmp))]
    findSubBitmapsEqual ::
        bmp  -- Super bitmap
     -> bmp  -- Sub bitmap
     -> Coordinates (BIndexType bmp)
     -> [(Coordinates (BIndexType bmp))]
    findEmbeddedBitmap ::
     (Integral i, Bitmap bmp)
     => [bmp]
     -> bmp  -- Super bitmap
     -> Coordinates (BIndexType bmp)  -- Coordinates relative to super bitmap
     -> Maybe (i, bmp)
                 -- ^ Find the first bitmap from the list that matches with
                 -- the area of the same size from the given coordinate in
                 -- the "super" bitmap (passed as the second argument)
                 -- down-right (the coordinate is the first pixel which is
                 -- the top-left most of the area to check).  The match
                 -- sub-bitmap and its index in the list are passed in
                 -- the opposite order given in this description to the
                 -- function, and the result is returned; if one is found.
                 -- If no match is found, 'Nothing' is returned.
                 --
                 -- The "sub" bitmaps are tested in order until a match is
                 -- found, and if one is, its index in the list is
                 -- returned.  Each pixel in every "sub" bitmap is specially
                 -- colored to represent a particular function.  These
                 -- colors are recognized:
                 --
                 -- - black / greatest intensity: the corresponding pixel in
                 -- the "super" bitmap based on position can be any color.
                 -- - white / least intensity: the corresponding pixel in the
                 -- super bitmap must be the same color as every other pixel
                 -- in the super bitmap that also corresponds to a white pixel.
                 -- - red / FF0000 / completely red: if there are white
                 -- pixels in the sub bitmap, the corresponding pixel of the
                 -- red pixel should be different from the color that
                 -- corresponds to the white pixels.
                 -- - green / 00FF00 / complete green: if there are white pixels
                 -- in the sub bitmap, the corresponding pixel of the
                 -- green pixel should not be similar from the color
                 -- that corresponds to the white pixels.  See
                 -- 'areColorsSimilar' to see whether two colors are
                 -- considered to be "similar".
                 --
                 -- The behaviour when any other pixel is encountered is
                 -- undefined.
                 --
                 -- When the dimensions of a sub bitmap are too large for the
                 -- super bitmap offset by the coordinates, where otherwise
                 -- some pixels of the sub bitmap would not have any
                 -- corresponding pixels in the super bitmap; then the sub
                 -- bitmap simply does not match.
                 --
                 -- This function makes OCR with a known and static font
                 -- more convenient to implement.

    findEmbeddedBitmapString ::
     (Integral i, Bitmap bmp)
     => ((i, bmp) -> a -> a)
     -> a
     -> [bmp]
     -> bmp
     -> Coordinates (BIndexType bmp)
     -> a -- 'foldr' equivalent of 'findEmbeddedBitmap' for a horizontal string of embedded bitmaps
          --
          -- This is particularly convenient for OCR with a static and known font with multiple characters.

    findPixel f b = findPixelOrder f b (0, 0)

    findPixelOrder f b = r'
        where r' i@(row, column)
                  | column > maxColumn =
                      r' (succ row, 0)
                  | row    > maxRow    =
                      Nothing
                  | f $ getPixel b i   =
                      Just i
                  | otherwise          =
                      r' (row, succ column)

              (width, height) = dimensions b
              maxRow    = abs . pred $ height
              maxColumn = abs . pred $ width

    findPixelEqual p = findPixelOrder (== p)

    findPixels f b = r'
        where (width, height) = dimensions b
              maxColumn = abs . pred $ width
              maxRow    = abs . pred $ height
              nextCoordinate (row, column)
                  | column >= maxColumn = (succ row, 0)
                  | otherwise           = (row, succ column)
              r' i      =
                  case findPixelOrder f b i of
                      (Just i'@(row, column)) ->
                          if row < maxRow || column < maxColumn
                              then i' : findPixels f b (nextCoordinate i')
                              else i' : []
                      (Nothing)               ->
                          []

    findPixelsEqual p b = r'
        where (width, height) = dimensions b
              maxColumn = abs . pred $ width
              maxRow    = abs . pred $ height
              nextCoordinate (row, column)
                  | column >= maxColumn = (succ row, 0)
                  | otherwise           = (row, succ column)
              r' i      =
                  case findPixelEqual p b i of
                      (Just i'@(row, column)) ->
                          if row < maxRow || column < maxColumn
                              then i' : findPixelsEqual p b (nextCoordinate i')
                              else i' : []
                      (Nothing)               ->
                          []

    findSubBitmap f super sub = findSubBitmapOrder f super sub (0, 0)

    findSubBitmapOrder f super sub = r'
        where r' i@(row, column)
                  | column > maxColumn =
                      r' (succ row, 0)
                  | row    > maxRow    =
                      Nothing
                  | matches (0, 0)     =
                      Just i
                  | otherwise          =
                      r' (row, succ column)
                  where matches offi@(offRow, offColumn)
                            | offColumn > maxOffColumn                                                        =
                                matches (succ offRow, 0)
                            | offRow    > maxOffRow                                                           =
                                True
                            | not $ f (getPixel super (row + offRow, column + offColumn)) (getPixel sub offi) =
                                False
                            | otherwise                                                                       =
                                matches (offRow, succ offColumn)

              (widthSuper, heightSuper)  = dimensions super
              (widthSub,   heightSub)    = dimensions sub
              (maxRow,     maxColumn)    = (heightSuper - heightSub, widthSuper - widthSub)
              (maxOffRow,  maxOffColumn) = (abs . pred $ heightSub, abs . pred $ widthSub)

    findSubBitmapEqual = findSubBitmapOrder (==)

    findSubBitmaps f super sub = r'
        where (widthSuper, heightSuper) = dimensions super
              (widthSub,   heightSub)   = dimensions sub
              (maxRow,     maxColumn)   = (heightSuper - heightSub, widthSuper - widthSub)
              nextCoordinate (row, column)
                  | column >= maxColumn = (succ row, 0)
                  | otherwise           = (row, succ column)
              r' i =
                  case findSubBitmapOrder f super sub i of
                      (Just i'@(row, column)) ->
                          if row < maxRow || column < maxColumn
                              then i' : findSubBitmaps f super sub (nextCoordinate i')
                              else i' : []
                      (Nothing)               ->
                          []

    findSubBitmapsEqual super sub = r'
        where (widthSuper, heightSuper) = dimensions super
              (widthSub,   heightSub)   = dimensions sub
              (maxRow,     maxColumn)   = (heightSuper - heightSub, widthSuper - widthSub)
              nextCoordinate (row, column)
                  | column >= maxColumn = (succ row, 0)
                  | otherwise           = (row, succ column)
              r' i =
                  case findSubBitmapEqual super sub i of
                      (Just i'@(row, column)) ->
                          if row < maxRow || column < maxColumn
                              then i' : findSubBitmapsEqual super sub (nextCoordinate i')
                              else i' : []
                      (Nothing)               ->
                          []

    findEmbeddedBitmap allEmbs super (row, column) = r' 0 allEmbs
        where pixAny  = matchPixelAny
              pixSame = matchPixelSame
              pixDif  = matchPixelDif
              pixDft  = matchPixelDifThreshold
              dimensionsSuper = dimensions super
              r' _ []     = Nothing
              r' n (e:es)
                  | True <- dimensionsFit dimensionsSuper (widthSub + column, heightSub + row)
                  , True <- matches Nothing [] [] (0, 0)
                      = Just $ (n, e)
                  | otherwise
                      = r' (succ n) es
                  -- difColors is necessary because red pixels could be encountered first, so we keep track of every color of every red pixel until we encounter a white pixel, and then we can empty the list after we check whether the first color is part of the list; for efficiency we always eliminate duplicates
                  where matches matchColor difColors dftColors offi@(offRow, offColumn)
                            | offColumn > maxOffColumn
                                = matches matchColor difColors dftColors (succ offRow, 0)
                            | offRow    > maxOffRow
                                = True
                            | (False, _,           _,          _)          <- posCondition
                                = False
                            | (_,     matchColor', difColors', dftColors') <- posCondition
                                = matches matchColor' difColors' dftColors' (offRow, succ offColumn)
                            where posCondition
                                      | subPixel == pixAny
                                          -- Any pixel (black in sub)
                                          = (True, matchColor, difColors, dftColors)
                                      | True               <- subPixel == pixSame
                                      , (Just matchColor') <- matchColor
                                          -- Match pixel (white in sub); matching color already set
                                          = (superPixel == matchColor', matchColor, difColors, dftColors)  -- difColors + dftColors should already be empty
                                      | True               <- subPixel == pixSame
                                          -- First match pixel (white in sub); record matching color
                                          = ((not $ superPixel `elem` difColors) && (not $ any (`areColorsSimilar` superPixel) dftColors), Just superPixel, [], [])
                                      | True               <- subPixel == pixDif
                                      , (Just matchColor') <- matchColor
                                          -- Dif pixel (completely red in sub); matching color found
                                          = (superPixel /= matchColor', matchColor, difColors, dftColors)  -- difColors + dftColors should already be empty
                                      | True               <- subPixel == pixDif
                                          -- Dif pixel (completely red in sub); matching color not yet found
                                          = (True, matchColor, nub $ superPixel : difColors, dftColors)
                                      | True               <- subPixel == pixDft
                                      , (Just matchColor') <- matchColor
                                          -- Dft pixel (completely green in sub); matching color found
                                          = (not $ superPixel `areColorsSimilar` matchColor', matchColor, difColors, dftColors)  -- difColors + dftColors should already be empty
                                      | True               <- subPixel == pixDft
                                          -- Dft pixel (completely green in sub); matching color not yet found
                                          = (True, matchColor, difColors, nub $ superPixel : dftColors)
                                      | otherwise
                                          -- undefined pixel in sub bitmap
                                          = (False, matchColor, difColors, dftColors)
                                      where superPixel = getPixel super (row + offRow, column + offColumn)
                                            subPixel   = getPixel e     offi

                        (widthSub,   heightSub)    = dimensions e
                        (maxOffRow,  maxOffColumn) = (abs . pred $ heightSub, abs . pred $ widthSub)

    findEmbeddedBitmapString f z allEmbs super = go
        where go pos@(row, column) =
                  case findEmbeddedBitmap allEmbs super pos of
                      (Nothing)         -> z
                      (Just r@(_, bmp)) -> r `f` go (row, column + (max 1 $ bitmapWidth bmp))

instance (Bitmap a) => BitmapSearchable a

-- | Binary similarity comparison
--
-- This function considers two colors to be "similar" if their difference
-- according to the CIE94 algorithm (see 'colorDifferenceCIE94') is less than
-- 23.
areColorsSimilar :: (Pixel p) => p -> p -> Bool
areColorsSimilar a b =
    let d :: Double
        d = colorDifferenceCIE94 a b
    in  d <= 23

-- | Approximate difference in color according to the CIE94 algorithm
colorDifferenceCIE94 :: (Pixel p, RealFloat n, Ord n) => p -> p -> n
colorDifferenceCIE94 pa pb =
    let sq x = x * x

        rgb_ra = (fromIntegral $ red   <: pa) / 255.0
        rgb_rb = (fromIntegral $ red   <: pb) / 255.0
        rgb_ga = (fromIntegral $ green <: pa) / 255.0
        rgb_gb = (fromIntegral $ green <: pb) / 255.0
        rgb_ba = (fromIntegral $ blue  <: pa) / 255.0
        rgb_bb = (fromIntegral $ blue  <: pb) / 255.0

        f x
            | x > 0.04045 = 100 * ((x + 0.055) / 1.055) ** 2.4
            | otherwise   = 100 * x / 12.92

        rgb_ra' = f rgb_ra
        rgb_rb' = f rgb_rb
        rgb_ga' = f rgb_ga
        rgb_gb' = f rgb_gb
        rgb_ba' = f rgb_ba
        rgb_bb' = f rgb_bb

        xa = 0.4124 * rgb_ra' + 0.3576 * rgb_ga' + 0.1805 * rgb_ba'
        xb = 0.4124 * rgb_rb' + 0.3576 * rgb_gb' + 0.1805 * rgb_bb'
        ya = 0.2126 * rgb_ra' + 0.7152 * rgb_ga' + 0.0722 * rgb_ba'
        yb = 0.2126 * rgb_rb' + 0.7152 * rgb_gb' + 0.0722 * rgb_bb'
        za = 0.0193 * rgb_ra' + 0.1192 * rgb_ga' + 0.9505 * rgb_ba'
        zb = 0.0193 * rgb_rb' + 0.1192 * rgb_gb' + 0.9505 * rgb_bb'


        g x
            | x > 0.008856 = x ** (1/3)
            | otherwise    = 7.787 * x + 16 / 116

        xa' = g $ xa / 95.047
        xb' = g $ xb / 95.047
        ya' = g $ ya / 100
        yb' = g $ yb / 100
        za' = g $ za / 108.883
        zb' = g $ zb / 108.883

        la = (116 * ya') - 16
        lb = (116 * yb') - 16
        aa = 500 * (xa' - ya')
        ab = 500 * (xb' - yb')
        ba = 200 * (ya' - za')
        bb = 200 * (yb' - zb')

        kl = 1
        k1 = 0.045
        k2 = 0.015

        h r
            | r >= 0 = r
            | otherwise = 2 * pi - (min 0 $ abs r)

        ca = sqrt (sq aa + sq ba)
        cb = sqrt (sq ab + sq bb)
        ha = h $ atan2 ba aa
        hb = h $ atan2 bb ab
    in  sqrt $ (sq $ (lb - la) / kl) + (sq $ (cb - ca) / (1 + k1 * ca)) + (sq $ (ha - hb) / (1 + k2 * ca))

-- | Default transparent pixel value; FF007E
defaultTransparentPixel :: (Pixel p) => p
defaultTransparentPixel =
    (red   =: 0xFF)
  . (green =: 0x00)
  . (blue  =: 0x7E)
  $ leastIntensity

matchPixelAny :: (Pixel p) => p
matchPixelAny = leastIntensity

matchPixelSame :: (Pixel p) => p
matchPixelSame = greatestIntensity

matchPixelDif :: (Pixel p) => p
matchPixelDif =
      (red   =: 0xFF)
    . (green =: 0x00)
    . (blue  =: 0x00)
    $ leastIntensity

matchPixelDifThreshold :: (Pixel p) => p
matchPixelDifThreshold =
      (red   =: 0x00)
    . (green =: 0xFF)
    . (blue  =: 0x00)
    $ leastIntensity
