module Data.Bitmap.Searchable
    ( BitmapSearchable(..)
    ) where

import Data.Bitmap.Class
import Data.Bitmap.Pixel

import Debug.Trace---------------------------------------------------
import Text.Printf

-- | Class for searchable bitmaps
--
-- Using the functions of the 'Bitmap' class,
-- default functions are be defined for each of
-- these; of course, implementations are free
-- to write more efficient versions.
class (Bitmap bmp) => BitmapSearchable bmp where
    findPixel ::
        (Pixel -> Bool)
     -> bmp
     -> Maybe (BIndexType bmp, BIndexType bmp)  -- ^ Scan each pixel until a match is found in no particular order
                                                --
                                                -- Implementations are free to choose an efficient implementation that
                                                -- searches in a different direction from that of 'findPixelOrder'.
                                                -- This function is often, but not necessarily always, the same as
                                                -- 'findPixelOrder'.
    findPixelOrder ::
        (Pixel -> Bool)
     -> bmp
     -> Maybe (BIndexType bmp, BIndexType bmp)  -- ^ Scan each pixel, row by row from the left, until a match is found
    findPixelEqual ::
        Pixel
     -> bmp
     -> Maybe (BIndexType bmp, BIndexType bmp)  -- ^ A more restricted version of 'findPixel' that is usually more efficient when exact equality is desired
                                                --
                                                -- NB: Pixels are only equal if their types are equal, not just their components.  For component
                                                -- equivalence, use 'eqPixelValue' and 'findPixel'.
    findSubBitmap ::
        (Pixel -> Pixel -> Bool)
     -> bmp  -- Super bitmap
     -> bmp  -- Sub bitmap
     -> Maybe (BIndexType bmp, BIndexType bmp)  -- ^ Search for where a sub-bitmap would match
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
    findSubBitmapEqual ::
        bmp  -- Super bitmap
     -> bmp  -- Sub bitmap
     -> Maybe (BIndexType bmp, BIndexType bmp)  -- ^ A more restricted version of 'findSubBitmap' that is usually more efficient when exact equality is desired

    findPixel = findPixelOrder

    findPixelOrder f b = r' (0, 0)
        where r' i@(row, column)
                  | f $ getPixel b i    = Just i
                  | column == maxColumn =
                      if row == maxRow
                          then
                              Nothing
                          else
                              r' (succ row, 0)
                  | otherwise =
                      r' (row, succ column)

              (width, height) = dimensions b
              (maxRow, maxColumn) = (abs . pred $ height, abs . pred $ width)

    findPixelEqual p = findPixel (== p)

    findSubBitmap f super sub = r' (0, 0)
        where r' i@(row, column)
                  | matches i           = Just i
                  | column >= maxColumn =
                      if row >= maxRow
                          then
                              Nothing
                          else
                              r' (succ row, 0)
                  | otherwise =
                      r' (row, succ column)

              (widthSuper, heightSuper) = dimensions super
              (widthSub,   heightSub)   = dimensions sub
              (maxRow,     maxColumn)   = (heightSuper - heightSub, widthSuper - widthSub)

              matches (superRow, superColumn) = r'' (0, 0)
                  where r'' subi@(offRow, offColumn)
                            | not $ f (getPixel super (superRow + offRow, superColumn + offColumn)) (getPixel sub subi) = trace (printf "nope (%d, %d); off (%d, %d)" (fromIntegral superRow :: Integer) (fromIntegral superColumn :: Integer) (fromIntegral offRow :: Integer) (fromIntegral offColumn :: Integer)) False
                            | offColumn >= (abs . pred $ widthSub) =
                                if offRow == (abs . pred $ heightSub)
                                    then True
                                    else r'' (succ offRow, 0)
                            | otherwise =
                                r'' (offRow, succ offColumn)

    findSubBitmapEqual = findSubBitmap (==)
