{- | Machine integer parsers.

Parsers use native byte order, unless stated otherwise.

TODO: The endianness code is currently lying. We blindly assume that our host
system is little-endian, and parse in big-endian by parsing normally then
"reversing" the resulting integer.
-}

module FlatParse.Basic.Integers
  (
  -- * Machine integer parsers (native byte order)
    getWord8, getWord16, getWord32, getWord64
  , getInt8,  getInt16,  getInt32,  getInt64
  , getWord, getInt

  -- * Machine integer parsers (explicit endianness)
  , getWord16le, getWord16be
  , getWord32le, getWord32be
  , getWord64le, getWord64be
  , getInt16le,  getInt16be
  , getInt32le,  getInt32be
  , getInt64le,  getInt64be

  -- * Machine integer parsers which assert value
  , getWord8Of

  -- * Machine integer CPS parsers
  , withWord8, withWord16, withWord32, withWord64
  , withInt8,  withInt16,  withInt32,  withInt64
  , withWord, withInt

  -- * Unsafe machine integer parsers
  , getWord8Unsafe

  -- * Unsafe machine integer parsers which assert value
  , getWord8OfUnsafe, getWord16OfUnsafe, getWord32OfUnsafe, getWord64OfUnsafe

  -- * Helper definitions
  , withSized#, withSizedUnsafe#
  , getSizedOfUnsafe#
  ) where

import FlatParse.Basic.Parser

import FlatParse.Common.Assorted ( word16ToInt16, word32ToInt32, word64ToInt64 )

import GHC.Exts
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
withSized#
    :: Int# -> (Addr# -> Int# -> a) -> (a -> Parser e r) -> Parser e r
withSized# size# indexOffAddr p = Parser \fp eob buf ->
    case size# <=# minusAddr# eob buf of
      0# -> Fail#
      _  -> runParser# (withSizedUnsafe# size# indexOffAddr p) fp eob buf
{-# inline withSized# #-}

-- | Unsafe helper for defining CPS parsers for types of a constant byte size
--   (i.e. machine integers).
--
-- The caller must guarantee that the input has enough bytes.
withSizedUnsafe#
    :: Int# -> (Addr# -> Int# -> a) -> (a -> Parser e r) -> Parser e r
withSizedUnsafe# size# indexOffAddr p = Parser \fp eob buf ->
  let a    = indexOffAddr buf 0#
      buf' = plusAddr# buf size#
  in  runParser# (p a) fp eob buf'
{-# inline withSizedUnsafe# #-}

-- | Parse any 'Word8' (CPS).
withWord8 :: (Word8 -> Parser e r) -> Parser e r
withWord8 p = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> let w# = indexWord8OffAddr# buf 0#
        in  runParser# (p (W8# w#)) fp eob (plusAddr# buf 1#)
{-# inline withWord8 #-}

-- | Parse any 'Word16' (native byte order) (CPS).
withWord16 :: (Word16 -> Parser e r) -> Parser e r
withWord16 = withSized# 2# (\a i -> W16# (indexWord16OffAddr# a i))
{-# inline withWord16 #-}

-- | Parse any 'Word32' (native byte order) (CPS).
withWord32 :: (Word32 -> Parser e r) -> Parser e r
withWord32 = withSized# 4# (\a i -> W32# (indexWord32OffAddr# a i))
{-# inline withWord32 #-}

-- | Parse any 'Word64' (native byte order) (CPS).
withWord64 :: (Word64 -> Parser e r) -> Parser e r
withWord64 = withSized# 8# (\a i -> W64# (indexWord64OffAddr# a i))
{-# inline withWord64 #-}

-- | Parse any 'Int8' (CPS).
withInt8 :: (Int8 -> Parser e r) -> Parser e r
withInt8 p = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> let i# = indexInt8OffAddr# buf 0#
        in  runParser# (p (I8# i#)) fp eob (plusAddr# buf 1#)
{-# inline withInt8 #-}

-- | Parse any 'Int16' (native byte order) (CPS).
withInt16 :: (Int16 -> Parser e r) -> Parser e r
withInt16 = withSized# 2# (\a i -> I16# (indexInt16OffAddr# a i))
{-# inline withInt16 #-}

-- | Parse any 'Int32' (native byte order) (CPS).
withInt32 :: (Int32 -> Parser e r) -> Parser e r
withInt32 = withSized# 4# (\a i -> I32# (indexInt32OffAddr# a i))
{-# inline withInt32 #-}

-- | Parse any 'Int64' (native byte order) (CPS).
withInt64 :: (Int64 -> Parser e r) -> Parser e r
withInt64 = withSized# 8# (\a i -> I64# (indexInt64OffAddr# a i))
{-# inline withInt64 #-}

-- | Parse any 'Word' (CPS).
--
-- TODO assumes 64-bit platform
withWord :: (Word -> Parser e r) -> Parser e r
withWord p = Parser \fp eob buf -> case 8# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> let w# = indexWordOffAddr# buf 0#
        in  runParser# (p (W# w#)) fp eob (plusAddr# buf 8#)
{-# inline withWord #-}

-- | Parse any 'Int' (CPS).
--
-- TODO assumes 64-bit platform
withInt :: (Int -> Parser e r) -> Parser e r
withInt p = Parser \fp eob buf -> case 8# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> let i# = indexIntOffAddr# buf 0#
        in  runParser# (p (I# i#)) fp eob (plusAddr# buf 8#)
{-# inline withInt #-}

--------------------------------------------------------------------------------

-- | Parse any 'Word8'.
getWord8 :: Parser e Word8
getWord8 = withWord8 pure
{-# inline getWord8 #-}

-- | Parse any 'Word16' (native byte order).
getWord16 :: Parser e Word16
getWord16 = withWord16 pure
{-# inline getWord16 #-}

-- | Parse any 'Word32' (native byte order).
getWord32 :: Parser e Word32
getWord32 = withWord32 pure
{-# inline getWord32 #-}

-- | Parse any 'Word64' (native byte order).
getWord64 :: Parser e Word64
getWord64 = withWord64 pure
{-# inline getWord64 #-}

-- | Parse any 'Int8'.
getInt8 :: Parser e Int8
getInt8 = withInt8 pure
{-# inline getInt8 #-}

-- | Parse any 'Int16' (native byte order).
getInt16 :: Parser e Int16
getInt16 = withInt16 pure
{-# inline getInt16 #-}

-- | Parse any 'Int32' (native byte order).
getInt32 :: Parser e Int32
getInt32 = withInt32 pure
{-# inline getInt32 #-}

-- | Parse any 'Int64' (native byte order).
getInt64 :: Parser e Int64
getInt64 = withInt64 pure
{-# inline getInt64 #-}

-- | Parse any 'Word'.
--
-- TODO 'withWord' assumes 64-bit platform
getWord :: Parser e Word
getWord = withWord pure
{-# inline getWord #-}

-- | Parse any 'Int'.
--
-- TODO 'withInt' assumes 64-bit platform
getInt :: Parser e Int
getInt = withInt pure
{-# inline getInt #-}

--------------------------------------------------------------------------------

-- | Parse any 'Word16' (little-endian).
getWord16le :: Parser e Word16
getWord16le = getWord16
{-# inline getWord16le #-}

-- | Parse any 'Word16' (big-endian).
getWord16be :: Parser e Word16
getWord16be = withWord16 (pure . byteSwap16)
{-# inline getWord16be #-}

-- | Parse any 'Word32' (little-endian).
getWord32le :: Parser e Word32
getWord32le = getWord32
{-# inline getWord32le #-}

-- | Parse any 'Word32' (big-endian).
getWord32be :: Parser e Word32
getWord32be = withWord32 (pure . byteSwap32)
{-# inline getWord32be #-}

-- | Parse any 'Word64' (little-endian).
getWord64le :: Parser e Word64
getWord64le = getWord64
{-# inline getWord64le #-}

-- | Parse any 'Word64' (big-endian).
getWord64be :: Parser e Word64
getWord64be = withWord64 (pure . byteSwap64)
{-# inline getWord64be #-}

-- | Parse any 'Int16' (little-endian).
getInt16le :: Parser e Int16
getInt16le = getInt16
{-# inline getInt16le #-}

-- | Parse any 'Int16' (big-endian).
getInt16be :: Parser e Int16
getInt16be = withWord16 (pure . word16ToInt16 . byteSwap16)
{-# inline getInt16be #-}

-- | Parse any 'Int32' (little-endian).
getInt32le :: Parser e Int32
getInt32le = getInt32
{-# inline getInt32le #-}

-- | Parse any 'Int32' (big-endian).
getInt32be :: Parser e Int32
getInt32be = withWord32 (pure . word32ToInt32 . byteSwap32)
{-# inline getInt32be #-}

-- | Parse any 'Int64' (little-endian).
getInt64le :: Parser e Int64
getInt64le = getInt64
{-# inline getInt64le #-}

-- | Parse any 'Int64' (big-endian).
getInt64be :: Parser e Int64
getInt64be = withWord64 (pure . word64ToInt64 . byteSwap64)
{-# inline getInt64be #-}

--------------------------------------------------------------------------------

-- | Unsafe helper for defining parsers for types of a constant byte size (i.e.
--   machine integers) which assert the parsed value's... value.
--
-- Call this with an @indexXYZOffAddr@ primop (e.g.
-- 'GHC.Exts.indexWord8OffAddr'), the size in bytes of the type you're parsing,
-- and the expected value to test the parsed value against.
--
-- The caller must guarantee that the input has enough bytes.
getSizedOfUnsafe# :: Eq a => Int# -> (Addr# -> Int# -> a) -> a -> Parser e ()
getSizedOfUnsafe# size# indexOffAddr aExpected =
    withSizedUnsafe# size# indexOffAddr go
  where
    go aParsed =
        if   aParsed == aExpected
        then pure ()
        else empty
{-# inline getSizedOfUnsafe# #-}

-- | Read the next 1 byte and assert its value as a 'Word8'.
getWord8Of :: Word8 -> Parser e ()
getWord8Of wExpected = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> let w# = indexWord8OffAddr# buf 0#
        in  if   W8# w# == wExpected
            then OK# () (plusAddr# buf 1#)
            else Fail#
{-# inline getWord8Of #-}

-- | Unsafely read the next 1 byte and assert its value as a 'Word8'.
--
-- The caller must guarantee that the input has enough bytes.
getWord8OfUnsafe :: Word8 -> Parser e ()
getWord8OfUnsafe = getSizedOfUnsafe# 1# (\a i -> W8# (indexWord8OffAddr# a i))
{-# inline getWord8OfUnsafe #-}

-- | Unsafely read the next 2 bytes and assert their value as a 'Word16'.
--
-- The caller must guarantee that the input has enough bytes.
getWord16OfUnsafe :: Word16 -> Parser e ()
getWord16OfUnsafe = getSizedOfUnsafe# 2# (\a i -> W16# (indexWord16OffAddr# a i))
{-# inline getWord16OfUnsafe #-}

-- | Unsafely read the next 4 bytes and assert their value as a 'Word32'.
--
-- The caller must guarantee that the input has enough bytes.
getWord32OfUnsafe :: Word32 -> Parser e ()
getWord32OfUnsafe = getSizedOfUnsafe# 4# (\a i -> W32# (indexWord32OffAddr# a i))
{-# inline getWord32OfUnsafe #-}

-- | Unsafely read the next 8 bytes and assert their value as a 'Word64'.
--
-- The caller must guarantee that the input has enough bytes.
getWord64OfUnsafe :: Word64 -> Parser e ()
getWord64OfUnsafe = getSizedOfUnsafe# 8# (\a i -> W64# (indexWord64OffAddr# a i))
{-# inline getWord64OfUnsafe #-}

--------------------------------------------------------------------------------

-- | Unsafely parse any 'Word8', without asserting the input is non-empty.
--
-- The caller must guarantee that the input has enough bytes.
getWord8Unsafe :: Parser e Word8
getWord8Unsafe = withSizedUnsafe# 1# (\a i -> W8# (indexWord8OffAddr# a i)) pure
{-# inline getWord8Unsafe #-}
