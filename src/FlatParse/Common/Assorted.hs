module FlatParse.Common.Assorted
  (
  -- * Compatibility
    shortInteger

  -- * 'Char' predicates
  , isDigit, isLatinLetter, isGreekLetter

  -- * UTF conversions
  , packUTF8, charToBytes, strToBytes, packBytes, splitBytes

  -- * Shortcuts
  , derefChar8#

  -- * Boxed integer coercions
  -- $boxed-integer-coercion
  , word16ToInt16
  , word32ToInt32
  , word64ToInt64

  -- * TODO
  , withPosInt#, withIntUnwrap#
  ) where

import Data.Bits
import Data.Char ( ord )
import Data.Foldable (foldl')
import GHC.Exts

import qualified Data.ByteString as B

import Data.Word
import Data.Int

#if MIN_VERSION_base(4,15,0)
import GHC.Num.Integer (Integer(..))
#else
import GHC.Integer.GMP.Internals (Integer(..))
#endif

-- Compatibility
--------------------------------------------------------------------------------

shortInteger :: Int# -> Integer
#if MIN_VERSION_base(4,15,0)
shortInteger = IS
#else
shortInteger = S#
#endif
{-# inline shortInteger #-}


-- Char predicates
--------------------------------------------------------------------------------

-- | @isDigit c = \'0\' <= c && c <= \'9\'@
isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'
{-# inline isDigit #-}

-- | @isLatinLetter c = (\'A\' <= c && c <= \'Z\') || (\'a\' <= c && c <= \'z\')@
isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
{-# inline isLatinLetter #-}

-- | @isGreekLetter c = (\'Α\' <= c && c <= \'Ω\') || (\'α\' <= c && c <= \'ω\')@
isGreekLetter :: Char -> Bool
isGreekLetter c = ('Α' <= c && c <= 'Ω') || ('α' <= c && c <= 'ω')
{-# inline isGreekLetter #-}

-- UTF conversions
--------------------------------------------------------------------------------

-- | Convert a `String` to an UTF-8-coded `B.ByteString`.
packUTF8 :: String -> B.ByteString
packUTF8 str = B.pack $ do
  c <- str
  w <- charToBytes c
  pure (fromIntegral w)

charToBytes :: Char -> [Word]
charToBytes c'
    | c <= 0x7f     = [fromIntegral c]
    | c <= 0x7ff    = [0xc0 .|. y, 0x80 .|. z]
    | c <= 0xffff   = [0xe0 .|. x, 0x80 .|. y, 0x80 .|. z]
    | c <= 0x10ffff = [0xf0 .|. w, 0x80 .|. x, 0x80 .|. y, 0x80 .|. z]
    | otherwise = error "Not a valid Unicode code point"
  where
    c = ord c'
    z = fromIntegral (c                 .&. 0x3f)
    y = fromIntegral (unsafeShiftR c 6  .&. 0x3f)
    x = fromIntegral (unsafeShiftR c 12 .&. 0x3f)
    w = fromIntegral (unsafeShiftR c 18 .&. 0x7)

strToBytes :: String -> [Word]
strToBytes = concatMap charToBytes
{-# inline strToBytes #-}

packBytes :: [Word] -> Word
packBytes = fst . foldl' go (0, 0) where
  go (acc, shift) w | shift == 64 = error "packBytes: too many bytes"
  go (acc, shift) w = (unsafeShiftL (fromIntegral w) shift .|. acc, shift+8)

-- TODO chunks into 8-bytes for 64-bit performance
splitBytes :: [Word] -> ([Word], [Word])
splitBytes ws = case quotRem (length ws) 8 of
  (0, _) -> (ws, [])
  (_, r) -> (as, chunk8s bs) where
              (as, bs) = splitAt r ws
              chunk8s [] = []
              chunk8s ws = let (as, bs) = splitAt 8 ws in
                           packBytes as : chunk8s bs

derefChar8# :: Addr# -> Char#
derefChar8# addr = indexCharOffAddr# addr 0#
{-# inline derefChar8# #-}

--------------------------------------------------------------------------------

{- $boxed-integer-coercion

These functions should be no-ops. They correspond to the similarly-named GHC 9.4
primops which work on unboxed integers.
-}

-- | Coerce a 'Word16' to 'Int16'.
word16ToInt16 :: Word16 -> Int16
word16ToInt16 = fromIntegral
{-# inline word16ToInt16 #-}

-- | Coerce a 'Word32' to 'Int32'.
word32ToInt32 :: Word32 -> Int32
word32ToInt32 = fromIntegral
{-# inline word32ToInt32 #-}

-- | Coerce a 'Word64' to 'Int64'.
word64ToInt64 :: Word64 -> Int64
word64ToInt64 = fromIntegral
{-# inline word64ToInt64 #-}

-- | Assert for the given 'Int#' that @n >= 0@, and pass it on to the given
--   function.
--
-- Throws a runtime error if given a negative integer.
withPosInt# :: Int# -> (Int# -> r) -> r
withPosInt# n# f = case n# >=# 0# of
  1# -> f n#
  _  -> error "FlatParse.Basic.Base.withPosInt#: negative integer"
{-# inline withPosInt# #-}

-- | Unwrap the 'Int#' from an 'Int' and apply it to the given function.
withIntUnwrap# :: (Int# -> r) -> Int -> r
withIntUnwrap# f (I# i#) = f i#
{-# inline withIntUnwrap# #-}
