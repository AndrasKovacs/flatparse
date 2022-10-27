{-# language UnboxedTuples #-}

module FlatParse.Basic.Internal where

import FlatParse.Basic.Parser

import GHC.Exts
import GHC.ForeignPtr

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

--------------------------------------------------------------------------------

-- | Save the parsing state, then run a parser, then restore the state.
lookahead :: Parser e a -> Parser e a
lookahead (Parser f) = Parser \fp eob s ->
  case f fp eob s of
    OK# a _ -> OK# a s
    x       -> x
{-# inline lookahead #-}

--------------------------------------------------------------------------------

-- | Check that the input has at least the given number of bytes.
ensureBytes# :: Int -> Parser e ()
ensureBytes# (I# len) = Parser \fp eob s ->
  case len  <=# minusAddr# eob s of
    1# -> OK# () s
    _  -> Fail#
{-# inline ensureBytes# #-}

scanPartial64# :: Int -> Word -> Parser e ()
scanPartial64# (I# len) (W# w) = Parser \fp eob s ->
  case indexWordOffAddr# s 0# of
    w' -> case uncheckedIShiftL# (8# -# len) 3# of
      sh -> case uncheckedShiftL# w' sh of
        w' -> case uncheckedShiftRL# w' sh of
          w' -> case eqWord# w w' of
            1# -> OK# () (plusAddr# s len)
            _  -> Fail#
{-# inline scanPartial64# #-}

-- | Decrease the current input position by the given number of bytes.
setBack# :: Int -> Parser e ()
setBack# (I# i) = Parser \fp eob s ->
  OK# () (plusAddr# s (negateInt# i))
{-# inline setBack# #-}

--------------------------------------------------------------------------------
-- Helpers for common internal operations

-- | Assert for the given 'Int#' that @n >= 0@, and pass it on to the given
--   function.
--
-- Throws a runtime error if given a negative integer.
withPosInt# :: Int# -> (Int# -> a) -> a
withPosInt# n# f = case n# >=# 0# of
  1# -> f n#
  _  -> error "FlatParse.Basic.Internal.withPosInt#: negative integer"
{-# inline withPosInt# #-}

-- | Run the given parser only if we have not yet reached the end of the buffer.
withNotEob :: Parser e a -> Parser e a
withNotEob (Parser p) = Parser \fp eob s -> case eqAddr# eob s of
  1# -> Fail#
  _  -> p fp eob s
{-# inline withNotEob #-}

--------------------------------------------------------------------------------
-- Low level unboxed combinators

-- | Read @n@ bytes as a 'ByteString'. Fails if newer than @n@ bytes are
--   available.
--
-- Throws a runtime error if given a negative integer.
take# :: Int# -> Parser e B.ByteString
take# n# = withPosInt# n# takeUnsafe#
{-# inline take# #-}

-- | Read @n@ bytes as a 'ByteString'. Fails if newer than @n@ bytes are
--   available.
--
-- Undefined behaviour if given a negative integer.
takeUnsafe# :: Int# -> Parser e B.ByteString
takeUnsafe# n# = Parser \fp eob s -> case n# <=# minusAddr# eob s of
  1# -> OK# (B.PS (ForeignPtr s fp) 0 (I# n#)) (plusAddr# s n#)
  _  -> Fail#
{-# inline takeUnsafe# #-}

-- | Skip forward @n@ bytes and run the given parser. Fails if fewer than @n@
--   bytes are available.
--
-- Throws a runtime error if given a negative integer.
atSkip# :: Int# -> Parser e a -> Parser e a
atSkip# os# p = withPosInt# os# (\n# -> atSkipUnsafe# n# p)
{-# inline atSkip# #-}

-- | Skip forward @n@ bytes and run the given parser. Fails if fewer than @n@
--   bytes are available.
--
-- Undefined behaviour if given a negative integer.
atSkipUnsafe# :: Int# -> Parser e a -> Parser e a
atSkipUnsafe# os# (Parser p) = Parser \fp eob s -> case os# <=# minusAddr# eob s of
  1# -> p fp eob (plusAddr# s os#)
  _  -> Fail#
{-# inline atSkipUnsafe# #-}

-- | Skip forward @n@ bytes. Fails if fewer than @n@ bytes are available.
--
-- Throws a runtime error if given a negative integer.
skip# :: Int# -> Parser e ()
skip# os# = atSkip# os# (pure ())
{-# inline skip# #-}
