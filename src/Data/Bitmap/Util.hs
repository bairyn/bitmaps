{-# LANGUAGE ScopedTypeVariables #-}

-- | This module is unstable; functions are not guaranteed to be the same or even to exist in future versions
--
-- It is intended primarily for use by this library itself.
module Data.Bitmap.Util
    ( tablespoon
    , subStr
    , padByte
    ) where

import Control.Exception
import qualified Data.String.Class as S
import Data.Word
import System.IO.Unsafe (unsafePerformIO)

handlers :: [Handler (Either String a)]
handlers = [ Handler $ \(e :: ArithException)   -> return . Left . show $ e
           , Handler $ \(e :: ArrayException)   -> return . Left . show $ e
           , Handler $ \(e :: ErrorCall)        -> return . Left . show $ e
           , Handler $ \(e :: PatternMatchFail) -> return . Left . show $ e
           , Handler $ \(e :: SomeException)    -> throwIO e
           ]

-- | Hack to catch "pureish" asynchronous errors
--
-- This is only used as a workaround to the binary library's shortcoming of
-- using asynchronous errors instead of pure error handling, and also zlib's
-- same shortcoming.
--
-- This function is similar to the @spoon@ package's @teaspoon@ function,
-- except that it can return more information when an exception is caught.
tablespoon :: a -> Either String a
tablespoon x = unsafePerformIO $ (Right `fmap` evaluate x) `catches` handlers

-- | Return a substring
--
-- 'subStr' @index@ @length@ returns @length@ characters from the string
-- starting at @index@, which starts at 0.
--
-- > subStr 1 2 "abcd" == "bc"
subStr :: (S.StringCells s) => Int -> Int -> s -> s
subStr index length_ = S.take length_ . S.drop index

padByte :: Word8
padByte = 0x00
