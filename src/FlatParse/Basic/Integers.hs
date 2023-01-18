{- | Machine integer parsers.

Functions tagged with @Unsafe@ generally do not check that the input has enough
bytes. These can occasionally be useful for designing efficient parsers where
you already have such a guarantee.

TODO: The endianness code is currently lying. We blindly assume that our host
system is little-endian, and parse in big-endian by parsing normally then
"reversing" the resulting integer.
-}

module FlatParse.Basic.Integers
  (
  -- * Native byte order
    anyWord8, anyWord16, anyWord32, anyWord64
  , anyInt8,  anyInt16,  anyInt32,  anyInt64
  , anyWord, anyInt

  -- * Explicit endianness
  , anyWord16le, anyWord16be
  , anyWord32le, anyWord32be
  , anyWord64le, anyWord64be
  , anyInt16le,  anyInt16be
  , anyInt32le,  anyInt32be
  , anyInt64le,  anyInt64be

  -- * Value assertions
  , word8

  -- * CPS parsers
  , withAnyWord8, withAnyWord16, withAnyWord32, withAnyWord64
  , withAnyInt8,  withAnyInt16,  withAnyInt32,  withAnyInt64
  , withAnyWord, withAnyInt

  -- * Unsafe
  -- $unsafe
  , anyWord8Unsafe

  -- ** Value assertions
  , word8Unsafe, word16Unsafe, word32Unsafe, word64Unsafe

  -- * Helper definitions
  , withAnySized#, withAnySizedUnsafe#
  , sizedUnsafe#
  ) where

import FlatParse.Basic.Parser
import FlatParse.Basic.Base ( withEnsure# )

import FlatParse.Common.Assorted ( word16ToInt16, word32ToInt32, word64ToInt64 )

import FlatParse.Common.GHCExts
import GHC.Word
import GHC.Int

import Control.Applicative ( Alternative(empty) )

--------------------------------------------------------------------------------

-- | Helper for defining CPS parsers for types of a constant byte size (i.e.
--   machine integers).
--
-- Call this with an @indexXYZOffAddr@ primop (e.g.
-- 'GHC.Exts.indexWord8OffAddr') and the size in bytes of the type you're
-- parsing.
withAnySized#
    :: Int# -> (Addr# -> Int# -> a) -> (a -> ParserT st e r) -> ParserT st e r
withAnySized# size# indexOffAddr p =
    withEnsure# size# (withAnySizedUnsafe# size# indexOffAddr p)
{-# inline withAnySized# #-}

-- | Unsafe helper for defining CPS parsers for types of a constant byte size
--   (i.e. machine integers).
--
-- Is really just syntactic sugar for applying the given parser and shifting the
-- buffer along.
--
-- The caller must guarantee that the input has enough bytes.
withAnySizedUnsafe#
    :: Int# -> (Addr# -> Int# -> a) -> (a -> ParserT st e r) -> ParserT st e r
withAnySizedUnsafe# size# indexOffAddr p = ParserT \fp eob buf st ->
-- TODO force? i.e. @let !a, !buf'@ ?
  let a    = indexOffAddr buf 0#
      buf' = plusAddr# buf size#
  in  runParserT# (p a) fp eob buf' st
{-# inline withAnySizedUnsafe# #-}

-- | Parse any 'Word8' (CPS).
withAnyWord8 :: (Word8 -> ParserT st e r) -> ParserT st e r
withAnyWord8 p = ParserT \fp eob buf st -> case eqAddr# eob buf of
  1# -> Fail# st
  _  -> let w# = indexWord8OffAddr# buf 0#
        in  runParserT# (p (W8# w#)) fp eob (plusAddr# buf 1#) st
{-# inline withAnyWord8 #-}

-- | Parse any 'Word16' (native byte order) (CPS).
withAnyWord16 :: (Word16 -> ParserT st e r) -> ParserT st e r
withAnyWord16 = withAnySized# 2# (\a i -> W16# (indexWord16OffAddr# a i))
{-# inline withAnyWord16 #-}

-- | Parse any 'Word32' (native byte order) (CPS).
withAnyWord32 :: (Word32 -> ParserT st e r) -> ParserT st e r
withAnyWord32 = withAnySized# 4# (\a i -> W32# (indexWord32OffAddr# a i))
{-# inline withAnyWord32 #-}

-- | Parse any 'Word64' (native byte order) (CPS).
withAnyWord64 :: (Word64 -> ParserT st e r) -> ParserT st e r
withAnyWord64 = withAnySized# 8# (\a i -> W64# (indexWord64OffAddr# a i))
{-# inline withAnyWord64 #-}

-- | Parse any 'Int8' (CPS).
withAnyInt8 :: (Int8 -> ParserT st e r) -> ParserT st e r
withAnyInt8 p = ParserT \fp eob buf st -> case eqAddr# eob buf of
  1# -> Fail# st
  _  -> let i# = indexInt8OffAddr# buf 0#
        in  runParserT# (p (I8# i#)) fp eob (plusAddr# buf 1#) st
{-# inline withAnyInt8 #-}

-- | Parse any 'Int16' (native byte order) (CPS).
withAnyInt16 :: (Int16 -> ParserT st e r) -> ParserT st e r
withAnyInt16 = withAnySized# 2# (\a i -> I16# (indexInt16OffAddr# a i))
{-# inline withAnyInt16 #-}

-- | Parse any 'Int32' (native byte order) (CPS).
withAnyInt32 :: (Int32 -> ParserT st e r) -> ParserT st e r
withAnyInt32 = withAnySized# 4# (\a i -> I32# (indexInt32OffAddr# a i))
{-# inline withAnyInt32 #-}

-- | Parse any 'Int64' (native byte order) (CPS).
withAnyInt64 :: (Int64 -> ParserT st e r) -> ParserT st e r
withAnyInt64 = withAnySized# 8# (\a i -> I64# (indexInt64OffAddr# a i))
{-# inline withAnyInt64 #-}

-- | Parse any 'Word' (native size) (CPS).
--
-- TODO assumes 64-bit platform
withAnyWord :: (Word -> ParserT st e r) -> ParserT st e r
withAnyWord p = ParserT \fp eob buf st -> case 8# <=# minusAddr# eob buf of
  0# -> Fail# st
  _  -> let w# = indexWordOffAddr# buf 0#
        in  runParserT# (p (W# w#)) fp eob (plusAddr# buf 8#) st
{-# inline withAnyWord #-}

-- | Parse any 'Int' (native size) (CPS).
--
-- TODO assumes 64-bit platform
withAnyInt :: (Int -> ParserT st e r) -> ParserT st e r
withAnyInt p = ParserT \fp eob buf st -> case 8# <=# minusAddr# eob buf of
  0# -> Fail# st
  _  -> let i# = indexIntOffAddr# buf 0#
        in  runParserT# (p (I# i#)) fp eob (plusAddr# buf 8#) st
{-# inline withAnyInt #-}

--------------------------------------------------------------------------------

-- | Parse any 'Word8'.
anyWord8 :: ParserT st e Word8
anyWord8 = withAnyWord8 pure
{-# inline anyWord8 #-}

-- | Parse any 'Word16' (native byte order).
anyWord16 :: ParserT st e Word16
anyWord16 = withAnyWord16 pure
{-# inline anyWord16 #-}

-- | Parse any 'Word32' (native byte order).
anyWord32 :: ParserT st e Word32
anyWord32 = withAnyWord32 pure
{-# inline anyWord32 #-}

-- | Parse any 'Word64' (native byte order).
anyWord64 :: ParserT st e Word64
anyWord64 = withAnyWord64 pure
{-# inline anyWord64 #-}

-- | Parse any 'Int8'.
anyInt8 :: ParserT st e Int8
anyInt8 = withAnyInt8 pure
{-# inline anyInt8 #-}

-- | Parse any 'Int16' (native byte order).
anyInt16 :: ParserT st e Int16
anyInt16 = withAnyInt16 pure
{-# inline anyInt16 #-}

-- | Parse any 'Int32' (native byte order).
anyInt32 :: ParserT st e Int32
anyInt32 = withAnyInt32 pure
{-# inline anyInt32 #-}

-- | Parse any 'Int64' (native byte order).
anyInt64 :: ParserT st e Int64
anyInt64 = withAnyInt64 pure
{-# inline anyInt64 #-}

-- | Parse any 'Word' (native size).
--
-- TODO 'withAnyWord' assumes 64-bit platform
anyWord :: ParserT st e Word
anyWord = withAnyWord pure
{-# inline anyWord #-}

-- | Parse any 'Int' (native size).
--
-- TODO 'withAnyInt' assumes 64-bit platform
anyInt :: ParserT st e Int
anyInt = withAnyInt pure
{-# inline anyInt #-}

--------------------------------------------------------------------------------

-- | Parse any 'Word16' (little-endian).
anyWord16le :: ParserT st e Word16
anyWord16le = anyWord16
{-# inline anyWord16le #-}

-- | Parse any 'Word16' (big-endian).
anyWord16be :: ParserT st e Word16
anyWord16be = withAnyWord16 (pure . byteSwap16)
{-# inline anyWord16be #-}

-- | Parse any 'Word32' (little-endian).
anyWord32le :: ParserT st e Word32
anyWord32le = anyWord32
{-# inline anyWord32le #-}

-- | Parse any 'Word32' (big-endian).
anyWord32be :: ParserT st e Word32
anyWord32be = withAnyWord32 (pure . byteSwap32)
{-# inline anyWord32be #-}

-- | Parse any 'Word64' (little-endian).
anyWord64le :: ParserT st e Word64
anyWord64le = anyWord64
{-# inline anyWord64le #-}

-- | Parse any 'Word64' (big-endian).
anyWord64be :: ParserT st e Word64
anyWord64be = withAnyWord64 (pure . byteSwap64)
{-# inline anyWord64be #-}

-- | Parse any 'Int16' (little-endian).
anyInt16le :: ParserT st e Int16
anyInt16le = anyInt16
{-# inline anyInt16le #-}

-- | Parse any 'Int16' (big-endian).
anyInt16be :: ParserT st e Int16
anyInt16be = withAnyWord16 (pure . word16ToInt16 . byteSwap16)
{-# inline anyInt16be #-}

-- | Parse any 'Int32' (little-endian).
anyInt32le :: ParserT st e Int32
anyInt32le = anyInt32
{-# inline anyInt32le #-}

-- | Parse any 'Int32' (big-endian).
anyInt32be :: ParserT st e Int32
anyInt32be = withAnyWord32 (pure . word32ToInt32 . byteSwap32)
{-# inline anyInt32be #-}

-- | Parse any 'Int64' (little-endian).
anyInt64le :: ParserT st e Int64
anyInt64le = anyInt64
{-# inline anyInt64le #-}

-- | Parse any 'Int64' (big-endian).
anyInt64be :: ParserT st e Int64
anyInt64be = withAnyWord64 (pure . word64ToInt64 . byteSwap64)
{-# inline anyInt64be #-}

--------------------------------------------------------------------------------

-- | Read the next 1 byte and assert its value as a 'Word8'.
word8 :: Word8 -> ParserT st e ()
word8 wExpected = ParserT \fp eob buf st -> case eqAddr# eob buf of
  1# -> Fail# st
  _  -> let w# = indexWord8OffAddr# buf 0#
        in  if   W8# w# == wExpected
            then OK# st () (plusAddr# buf 1#)
            else Fail# st
{-# inline word8 #-}

--------------------------------------------------------------------------------

-- | Unsafe helper for defining parsers for types of a constant byte size (i.e.
--   machine integers) which assert the parsed value's... value.
--
-- Call this with an @indexXYZOffAddr@ primop (e.g.
-- 'GHC.Exts.indexWord8OffAddr'), the size in bytes of the type you're parsing,
-- and the expected value to test the parsed value against.
--
-- The caller must guarantee that the input has enough bytes.
sizedUnsafe# :: Eq a => Int# -> (Addr# -> Int# -> a) -> a -> ParserT st e ()
sizedUnsafe# size# indexOffAddr aExpected =
    withAnySizedUnsafe# size# indexOffAddr go
  where
    go aParsed =
        if   aParsed == aExpected
        then pure ()
        else empty
{-# inline sizedUnsafe# #-}

{- $unsafe
These unsafe parsers and helpers may be useful for efficient parsing in special
situations e.g. you already know that the input has enough bytes. You should
only use them if you can assert their necessary guarantees (see the individual
function documentation).
-}

-- | Unsafely read the next 1 byte and assert its value as a 'Word8'.
--
-- The caller must guarantee that the input has enough bytes.
word8Unsafe :: Word8 -> ParserT st e ()
word8Unsafe = sizedUnsafe# 1# (\a i -> W8# (indexWord8OffAddr# a i))
{-# inline word8Unsafe #-}

-- | Unsafely read the next 2 bytes and assert their value as a 'Word16'
--   (native byte order).
--
-- The caller must guarantee that the input has enough bytes.
word16Unsafe :: Word16 -> ParserT st e ()
word16Unsafe = sizedUnsafe# 2# (\a i -> W16# (indexWord16OffAddr# a i))
{-# inline word16Unsafe #-}

-- | Unsafely read the next 4 bytes and assert their value as a 'Word32'.
--   (native byte order).
--
-- The caller must guarantee that the input has enough bytes.
word32Unsafe :: Word32 -> ParserT st e ()
word32Unsafe = sizedUnsafe# 4# (\a i -> W32# (indexWord32OffAddr# a i))
{-# inline word32Unsafe #-}

-- | Unsafely read the next 8 bytes and assert their value as a 'Word64'.
--   (native byte order).
--
-- The caller must guarantee that the input has enough bytes.
word64Unsafe :: Word64 -> ParserT st e ()
word64Unsafe = sizedUnsafe# 8# (\a i -> W64# (indexWord64OffAddr# a i))
{-# inline word64Unsafe #-}

--------------------------------------------------------------------------------

-- | Unsafely parse any 'Word8', without asserting the input is non-empty.
--
-- The caller must guarantee that the input has enough bytes.
anyWord8Unsafe :: ParserT st e Word8
anyWord8Unsafe = withAnySizedUnsafe# 1# (\a i -> W8# (indexWord8OffAddr# a i)) pure
{-# inline anyWord8Unsafe #-}
