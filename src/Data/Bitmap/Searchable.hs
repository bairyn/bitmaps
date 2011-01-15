module Data.Bitmap.Searchable
    ( BitmapSearchable(..)
    , defaultTransparentPixel
    ) where

import Control.Monad.Record
import Data.Bitmap.Class
import Data.Bitmap.Pixel
import Data.Bitmap.Types

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
     -> Maybe (Coordinates (BIndexType bmp))    -- ^ A more restricted version of 'findPixel' that is usually more efficient when exact equality is desired
    findPixels ::
        (BPixelType bmp -> Bool)
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
     -> Maybe (Coordinates (BIndexType bmp))    -- ^ A more restricted version of 'findSubBitmap' that is usually more efficient when exact equality is desired
    findSubBitmaps ::
        (BPixelType bmp -> BPixelType bmp -> Bool)
     -> bmp  -- Super bitmap
     -> bmp  -- Sub bitmap
     -> Coordinates (BIndexType bmp)
     -> [(Coordinates (BIndexType bmp))]

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

-- | Default transparent pixel value; FF007E
defaultTransparentPixel :: (Pixel p) => p
defaultTransparentPixel =
    (red   =: 0xFF)
  . (green =: 0x00)
  . (blue  =: 0x7E)
  $ leastIntensity
