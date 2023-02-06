module FlatParse.Common.Strings where

import Data.Bits
import Data.Foldable (foldl')

import Data.Word
import Data.Int

import qualified Data.Char

-- | @isDigit c = \'0\' <= c && c <= \'9\'@
-- TODO exists in Data.Char, but maybe loses inlining
isDigit :: Char -> Bool
--isDigit c = '0' <= c && c <= '9'
isDigit = Data.Char.isDigit
{-# inline isDigit #-}

-- | @isAsciiLetter c = (\'A\' <= c && c <= \'Z\') || (\'a\' <= c && c <= \'z\')@
-- TODO exists in Data.Char, but maybe loses inlining
isAsciiLetter :: Char -> Bool
--isAsciiLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
isAsciiLetter c = Data.Char.isAsciiUpper c || Data.Char.isAsciiLower c
{-# inline isAsciiLetter #-}

-- | @isGreekLetter c = (\'Α\' <= c && c <= \'Ω\') || (\'α\' <= c && c <= \'ω\')@
isGreekLetter :: Char -> Bool
isGreekLetter c = ('Α' <= c && c <= 'Ω') || ('α' <= c && c <= 'ω')
{-# inline isGreekLetter #-}

-- UTF conversions
--------------------------------------------------------------------------------

packBytes :: [Word] -> Word
packBytes = fst . foldl' go (0, 0) where
  go (acc, shift) w | shift == 64 = error "packWords: too many bytes"
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
