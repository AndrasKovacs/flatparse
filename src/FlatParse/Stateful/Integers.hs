-- | Machine integer parsers.

module FlatParse.Stateful.Integers
  (
  -- * Native byte order
    anyWord8, anyWord16, anyWord32, anyWord64
  , anyInt8,  anyInt16,  anyInt32,  anyInt64
  , anyWord, anyInt

  -- * Explicit endianness
  -- $explicit-endianness
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

import FlatParse.Stateful.Parser
import FlatParse.Stateful.Base ( withEnsure# )

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
    :: Int# -> (Addr# -> Int# -> a) -> (a -> ParserT st r e ret)
    -> ParserT st r e ret
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
    :: Int# -> (Addr# -> Int# -> a) -> (a -> ParserT st r e ret)
    -> ParserT st r e ret
withAnySizedUnsafe# size# indexOffAddr p = ParserT \fp !r eob buf n st ->
-- TODO force? i.e. @let !a, !buf'@ ?
  let a    = indexOffAddr buf 0#
      buf' = plusAddr# buf size#
  in  runParserT# (p a) fp r eob buf' n st
{-# inline withAnySizedUnsafe# #-}

-- | Parse any 'Word8' (CPS).
withAnyWord8 :: (Word8 -> ParserT st r e ret) -> ParserT st r e ret
withAnyWord8 p = ParserT \fp !r eob buf n st -> case eqAddr# eob buf of
  1# -> Fail# st
  _  -> let w# = indexWord8OffAddr# buf 0#
        in  runParserT# (p (W8# w#)) fp r eob (plusAddr# buf 1#) n st
{-# inline withAnyWord8 #-}

-- | Parse any 'Word16' (native byte order) (CPS).
withAnyWord16 :: (Word16 -> ParserT st r e ret) -> ParserT st r e ret
withAnyWord16 = withAnySized# 2# (\a i -> W16# (indexWord16OffAddr# a i))
{-# inline withAnyWord16 #-}

-- | Parse any 'Word32' (native byte order) (CPS).
withAnyWord32 :: (Word32 -> ParserT st r e ret) -> ParserT st r e ret
withAnyWord32 = withAnySized# 4# (\a i -> W32# (indexWord32OffAddr# a i))
{-# inline withAnyWord32 #-}

-- | Parse any 'Word64' (native byte order) (CPS).
withAnyWord64 :: (Word64 -> ParserT st r e ret) -> ParserT st r e ret
withAnyWord64 = withAnySized# 8# (\a i -> W64# (indexWord64OffAddr# a i))
{-# inline withAnyWord64 #-}

-- | Parse any 'Int8' (CPS).
withAnyInt8 :: (Int8 -> ParserT st r e ret) -> ParserT st r e ret
withAnyInt8 p = ParserT \fp !r eob buf n st -> case eqAddr# eob buf of
  1# -> Fail# st
  _  -> let i# = indexInt8OffAddr# buf 0#
        in  runParserT# (p (I8# i#)) fp r eob (plusAddr# buf 1#) n st
{-# inline withAnyInt8 #-}

-- | Parse any 'Int16' (native byte order) (CPS).
withAnyInt16 :: (Int16 -> ParserT st r e ret) -> ParserT st r e ret
withAnyInt16 = withAnySized# 2# (\a i -> I16# (indexInt16OffAddr# a i))
{-# inline withAnyInt16 #-}

-- | Parse any 'Int32' (native byte order) (CPS).
withAnyInt32 :: (Int32 -> ParserT st r e ret) -> ParserT st r e ret
withAnyInt32 = withAnySized# 4# (\a i -> I32# (indexInt32OffAddr# a i))
{-# inline withAnyInt32 #-}

-- | Parse any 'Int64' (native byte order) (CPS).
withAnyInt64 :: (Int64 -> ParserT st r e ret) -> ParserT st r e ret
withAnyInt64 = withAnySized# 8# (\a i -> I64# (indexInt64OffAddr# a i))
{-# inline withAnyInt64 #-}

-- | Parse any 'Word' (native size) (CPS).
--
-- TODO assumes 64-bit platform
withAnyWord :: (Word -> ParserT st r e ret) -> ParserT st r e ret
withAnyWord p = ParserT \fp !r eob buf n st -> case 8# <=# minusAddr# eob buf of
  0# -> Fail# st
  _  -> let w# = indexWordOffAddr# buf 0#
        in  runParserT# (p (W# w#)) fp r eob (plusAddr# buf 8#) n st
{-# inline withAnyWord #-}

-- | Parse any 'Int' (native size) (CPS).
--
-- TODO assumes 64-bit platform
withAnyInt :: (Int -> ParserT st r e ret) -> ParserT st r e ret
withAnyInt p = ParserT \fp !r eob buf n st -> case 8# <=# minusAddr# eob buf of
  0# -> Fail# st
  _  -> let i# = indexIntOffAddr# buf 0#
        in  runParserT# (p (I# i#)) fp r eob (plusAddr# buf 8#) n st
{-# inline withAnyInt #-}

--------------------------------------------------------------------------------

-- | Parse any 'Word8'.
anyWord8 :: ParserT st r e Word8
anyWord8 = withAnyWord8 pure
{-# inline anyWord8 #-}

-- | Parse any 'Word16' (native byte order).
anyWord16 :: ParserT st r e Word16
anyWord16 = withAnyWord16 pure
{-# inline anyWord16 #-}

-- | Parse any 'Word32' (native byte order).
anyWord32 :: ParserT st r e Word32
anyWord32 = withAnyWord32 pure
{-# inline anyWord32 #-}

-- | Parse any 'Word64' (native byte order).
anyWord64 :: ParserT st r e Word64
anyWord64 = withAnyWord64 pure
{-# inline anyWord64 #-}

-- | Parse any 'Int8'.
anyInt8 :: ParserT st r e Int8
anyInt8 = withAnyInt8 pure
{-# inline anyInt8 #-}

-- | Parse any 'Int16' (native byte order).
anyInt16 :: ParserT st r e Int16
anyInt16 = withAnyInt16 pure
{-# inline anyInt16 #-}

-- | Parse any 'Int32' (native byte order).
anyInt32 :: ParserT st r e Int32
anyInt32 = withAnyInt32 pure
{-# inline anyInt32 #-}

-- | Parse any 'Int64' (native byte order).
anyInt64 :: ParserT st r e Int64
anyInt64 = withAnyInt64 pure
{-# inline anyInt64 #-}

-- | Parse any 'Word' (native size).
--
-- TODO 'withAnyWord' assumes 64-bit platform
anyWord :: ParserT st r e Word
anyWord = withAnyWord pure
{-# inline anyWord #-}

-- | Parse any 'Int' (native size).
--
-- TODO 'withAnyInt' assumes 64-bit platform
anyInt :: ParserT st r e Int
anyInt = withAnyInt pure
{-# inline anyInt #-}

--------------------------------------------------------------------------------

{- $explicit-endianness
Native endianness parsers are used where possible. For non-native endianness
parsers, we parse then use the corresponding @byteSwapX@ function. On x86, this
is inlined as a single @BSWAP@ instruction.
-}

-- | Parse any 'Word16' (little-endian).
anyWord16le :: ParserT st r e Word16
#ifdef WORDS_BIGENDIAN
anyWord16le = withAnyWord16 (pure . byteSwap16)
#else
anyWord16le = anyWord16
#endif
{-# inline anyWord16le #-}

-- | Parse any 'Word16' (big-endian).
anyWord16be :: ParserT st r e Word16
#ifdef WORDS_BIGENDIAN
anyWord16be = anyWord16
#else
anyWord16be = withAnyWord16 (pure . byteSwap16)
#endif
{-# inline anyWord16be #-}

-- | Parse any 'Word32' (little-endian).
anyWord32le :: ParserT st r e Word32
#ifdef WORDS_BIGENDIAN
anyWord32le = withAnyWord32 (pure . byteSwap32)
#else
anyWord32le = anyWord32
#endif
{-# inline anyWord32le #-}

-- | Parse any 'Word32' (big-endian).
anyWord32be :: ParserT st r e Word32
#ifdef WORDS_BIGENDIAN
anyWord32be = anyWord32
#else
anyWord32be = withAnyWord32 (pure . byteSwap32)
#endif
{-# inline anyWord32be #-}

-- | Parse any 'Word64' (little-endian).
anyWord64le :: ParserT st r e Word64
#ifdef WORDS_BIGENDIAN
anyWord64le = withAnyWord64 (pure . byteSwap64)
#else
anyWord64le = anyWord64
#endif
{-# inline anyWord64le #-}

-- | Parse any 'Word64' (big-endian).
anyWord64be :: ParserT st r e Word64
#ifdef WORDS_BIGENDIAN
anyWord64be = anyWord64
#else
anyWord64be = withAnyWord64 (pure . byteSwap64)
#endif
{-# inline anyWord64be #-}

-- | Parse any 'Int16' (little-endian).
anyInt16le :: ParserT st r e Int16
#ifdef WORDS_BIGENDIAN
anyInt16le = withAnyWord16 (pure . word16ToInt16 . byteSwap16)
#else
anyInt16le = anyInt16
#endif
{-# inline anyInt16le #-}

-- | Parse any 'Int16' (big-endian).
anyInt16be :: ParserT st r e Int16
#ifdef WORDS_BIGENDIAN
anyInt16be = anyInt16
#else
anyInt16be = withAnyWord16 (pure . word16ToInt16 . byteSwap16)
#endif
{-# inline anyInt16be #-}

-- | Parse any 'Int32' (little-endian).
anyInt32le :: ParserT st r e Int32
#ifdef WORDS_BIGENDIAN
anyInt32le = withAnyWord32 (pure . word32ToInt32 . byteSwap32)
#else
anyInt32le = anyInt32
#endif
{-# inline anyInt32le #-}

-- | Parse any 'Int32' (big-endian).
anyInt32be :: ParserT st r e Int32
#ifdef WORDS_BIGENDIAN
anyInt32be = anyInt32
#else
anyInt32be = withAnyWord32 (pure . word32ToInt32 . byteSwap32)
#endif
{-# inline anyInt32be #-}

-- | Parse any 'Int64' (little-endian).
anyInt64le :: ParserT st r e Int64
#ifdef WORDS_BIGENDIAN
anyInt64le = withAnyWord64 (pure . word64ToInt64 . byteSwap64)
#else
anyInt64le = anyInt64
#endif
{-# inline anyInt64le #-}

-- | Parse any 'Int64' (big-endian).
anyInt64be :: ParserT st r e Int64
#ifdef WORDS_BIGENDIAN
anyInt64be = anyInt64
#else
anyInt64be = withAnyWord64 (pure . word64ToInt64 . byteSwap64)
#endif
{-# inline anyInt64be #-}

--------------------------------------------------------------------------------

-- | Read the next 1 byte and assert its value as a 'Word8'.
word8 :: Word8 -> ParserT st r e ()
word8 wExpected = ParserT \fp !r eob buf n st -> case eqAddr# eob buf of
  1# -> Fail# st
  _  -> let w# = indexWord8OffAddr# buf 0#
        in  if   W8# w# == wExpected
            then OK# st () (plusAddr# buf 1#) n
            else Fail# st
{-# inline word8 #-}

--------------------------------------------------------------------------------

{- $unsafe
These unsafe parsers and helpers may be useful for efficient parsing in special
situations e.g. you already know that the input has enough bytes. You should
only use them if you can assert their necessary guarantees (see the individual
function documentation).
-}

-- | Unsafe helper for defining parsers for types of a constant byte size (i.e.
--   machine integers) which assert the parsed value's... value.
--
-- Call this with an @indexXYZOffAddr@ primop (e.g.
-- 'GHC.Exts.indexWord8OffAddr'), the size in bytes of the type you're parsing,
-- and the expected value to test the parsed value against.
--
-- The caller must guarantee that the input has enough bytes.
sizedUnsafe# :: Eq a => Int# -> (Addr# -> Int# -> a) -> a -> ParserT st r e ()
sizedUnsafe# size# indexOffAddr aExpected =
    withAnySizedUnsafe# size# indexOffAddr go
  where
    go aParsed =
        if   aParsed == aExpected
        then pure ()
        else empty
{-# inline sizedUnsafe# #-}

-- | Unsafely read the next 1 byte and assert its value as a 'Word8'.
--
-- The caller must guarantee that the input has enough bytes.
word8Unsafe :: Word8 -> ParserT st r e ()
word8Unsafe = sizedUnsafe# 1# (\a i -> W8# (indexWord8OffAddr# a i))
{-# inline word8Unsafe #-}

-- | Unsafely read the next 2 bytes and assert their value as a 'Word16'
--   (native byte order).
--
-- The caller must guarantee that the input has enough bytes.
word16Unsafe :: Word16 -> ParserT st r e ()
word16Unsafe = sizedUnsafe# 2# (\a i -> W16# (indexWord16OffAddr# a i))
{-# inline word16Unsafe #-}

-- | Unsafely read the next 4 bytes and assert their value as a 'Word32'.
--   (native byte order).
--
-- The caller must guarantee that the input has enough bytes.
word32Unsafe :: Word32 -> ParserT st r e ()
word32Unsafe = sizedUnsafe# 4# (\a i -> W32# (indexWord32OffAddr# a i))
{-# inline word32Unsafe #-}

-- | Unsafely read the next 8 bytes and assert their value as a 'Word64'.
--   (native byte order).
--
-- The caller must guarantee that the input has enough bytes.
word64Unsafe :: Word64 -> ParserT st r e ()
word64Unsafe = sizedUnsafe# 8# (\a i -> W64# (indexWord64OffAddr# a i))
{-# inline word64Unsafe #-}

--------------------------------------------------------------------------------

-- | Unsafely parse any 'Word8', without asserting the input is non-empty.
--
-- The caller must guarantee that the input has enough bytes.
anyWord8Unsafe :: ParserT st r e Word8
anyWord8Unsafe = withAnySizedUnsafe# 1# (\a i -> W8# (indexWord8OffAddr# a i)) pure
{-# inline anyWord8Unsafe #-}
