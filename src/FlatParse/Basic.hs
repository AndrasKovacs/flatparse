{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
This module implements a `Parser` supporting custom error types.  If you need efficient indentation
parsing, use "FlatParse.Stateful" instead.

Many internals are exposed for hacking on and extending. These are generally
denoted by a @#@ hash suffix.
-}

module FlatParse.Basic (
  -- * Parser type
    module FlatParse.Basic.Parser

  -- * Running parsers
  , Result(..)
  , runParser
  , runParserS
  , runParserIO
  , runParserST

  -- * Parsers
  , module FlatParse.Basic.Base
  , module FlatParse.Basic.Bytes

  -- ** Position
  , module FlatParse.Common.Position
  , getPos
  , setPos
  , spanOf
  , withSpan
  , byteStringOf
  , withByteString
  , inSpan

  -- ** Text
  , module FlatParse.Basic.Text
  , module FlatParse.Basic.Switch

  -- ** Machine integers
  , module FlatParse.Basic.Integers

  -- * Unsafe
  , module FlatParse.Basic.Addr
  , unsafeLiftIO

  -- * TODO
  , Control.Applicative.empty

  -- * TODO possibly remove
  , Common.packUTF8

  ) where

import Prelude hiding ( take )

import qualified Control.Applicative
import GHC.IO (IO(..))
import GHC.Exts
import GHC.ForeignPtr
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B

import FlatParse.Basic.Parser
import FlatParse.Basic.Base
import FlatParse.Basic.Integers
import FlatParse.Basic.Bytes
import FlatParse.Basic.Text
import FlatParse.Basic.Switch
import FlatParse.Basic.Addr
import FlatParse.Common.Position
import qualified FlatParse.Common.Assorted as Common
import qualified FlatParse.Common.Numbers  as Common

-- | Higher-level boxed data type for parsing results.
data Result e a =
    OK a !(B.ByteString)  -- ^ Contains return value and unconsumed input.
  | Fail                  -- ^ Recoverable-by-default failure.
  | Err !e                -- ^ Unrecoverble-by-default error.
  deriving Show

instance Functor (Result e) where
  fmap f (OK a s) = let !b = f a in OK b s
  fmap f r        = unsafeCoerce# r
  {-# inline fmap #-}
  (<$) a (OK _ s) = OK a s
  (<$) _ r        = unsafeCoerce# r
  {-# inline (<$) #-}

-- | Embed an IO action in a 'ParserT'. This is slightly safer than 'unsafePerformIO' because
-- it will sequenced correctly with respect to the surrounding actions, and its execution is guaranteed.
unsafeLiftIO :: IO a -> ParserT st e a
unsafeLiftIO io = ParserT \fp eob s st ->
                   let !a = unsafePerformIO io
                   in OK# st a s
{-# inline unsafeLiftIO #-}

--------------------------------------------------------------------------------

-- | Run a parser.
runParser :: Parser e a -> B.ByteString -> Result e a
runParser (ParserT f) b@(B.PS (ForeignPtr _ fp) _ (I# len)) = unsafePerformIO $
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let end = plusAddr# buf len
    pure case f fp end buf proxy# of
      OK# _st a s -> let offset = minusAddr# s buf
                     in OK a (B.drop (I# offset) b)

      Err# _st e -> Err e
      Fail# _st  -> Fail
{-# noinline runParser #-}
-- We mark this as noinline to allow power users to safely do unsafe state token coercions.
-- Details are discussed in https://github.com/AndrasKovacs/flatparse/pull/34#issuecomment-1326999390

-- | Run an ST based parser
runParserST :: (forall s. ParserST s e a) -> B.ByteString -> Result e a
runParserST pst buf = unsafeDupablePerformIO (runParserIO pst buf)
{-# inlinable runParserST #-}

-- | Run an IO based parser
runParserIO :: ParserIO e a -> B.ByteString -> IO (Result e a)
runParserIO (ParserT f) b@(B.PS (ForeignPtr _ fp) _ (I# len)) = do
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let end = plusAddr# buf len
    IO \st -> case f fp end buf st of
      OK# rw' a s ->  let offset = minusAddr# s buf
                      in (# rw', OK a (B.drop (I# offset) b) #)

      Err# rw' e ->  (# rw', Err e #)
      Fail# rw'  ->  (# rw', Fail #)
{-# inlinable runParserIO #-}

--------------------------------------------------------------------------------

-- | Parse a given `B.ByteString`. If the bytestring is statically known, consider using 'bytes' instead.
byteString :: B.ByteString -> ParserT st e ()
byteString (B.PS (ForeignPtr bs fcontent) _ (I# len)) =

  let go64 :: Addr# -> Addr# -> Addr# -> State# RealWorld -> Res# (State# RealWorld) e ()
      go64 bs bsend s rw =
        let bs' = plusAddr# bs 8# in
        case gtAddr# bs' bsend of
          1# -> go8 bs bsend s rw
#if MIN_VERSION_base(4,17,0)
          _  -> case eqWord64# (indexWord64OffAddr# bs 0#) (indexWord64OffAddr# s 0#) of
#else
          _  -> case eqWord# (indexWord64OffAddr# bs 0#) (indexWord64OffAddr# s 0#) of
#endif
            1# -> go64 bs' bsend (plusAddr# s 8#) rw
            _  -> Fail# rw

      go8 :: Addr# -> Addr# -> Addr# -> State# RealWorld -> Res# (State# RealWorld) e ()
      go8 bs bsend s rw = case ltAddr# bs bsend of
#if MIN_VERSION_base(4,16,0)
        1# -> case eqWord8# (indexWord8OffAddr# bs 0#) (indexWord8OffAddr# s 0#) of
#else
        1# -> case eqWord# (indexWord8OffAddr# bs 0#) (indexWord8OffAddr# s 0#) of
#endif
          1# -> go8 (plusAddr# bs 1#) bsend (plusAddr# s 1#) rw
          _  -> Fail# rw
        _  -> OK# rw () s

      go :: Addr# -> Addr# -> Addr# -> State# RealWorld -> Res# (State# RealWorld) e ()
      go bs bsend s rw = case go64 bs bsend s rw of
        (# rw', res #) -> case touch# fcontent rw' of
          rw'' -> (# rw'', res #)

  in ParserT \fp eob s st ->
      case len <=# minusAddr# eob s of
           1# -> case runRW# (go bs (plusAddr# bs len) s) of
             (# rw, a #) -> (# st, a #)
           _  -> Fail# st
{-# inline byteString #-}

--------------------------------------------------------------------------------

-- | Run a parser zero or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
many :: ParserT st e a -> ParserT st e [a]
many (ParserT f) = ParserT go where
  go fp eob s st = case f fp eob s st of
    OK# st' a s -> case go fp eob s st' of
                    OK# st'' as s -> OK# st'' (a:as) s
                    x        -> x
    Fail# st'  -> OK# st' [] s
    Err# st' e -> Err# st' e
{-# inline many #-}

-- | Run a parser one or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
some :: ParserT st e a -> ParserT st e [a]
some p = (:) <$> p <*> many p
{-# inline some #-}

--------------------------------------------------------------------------------

-- | Get the current position in the input.
getPos :: ParserT st e Pos
getPos = ParserT \fp eob s st -> OK# st (addrToPos# eob s) s
{-# inline getPos #-}

-- | Set the input position.
--
-- Warning: this can result in crashes if the position points outside the
-- current buffer. It is always safe to 'setPos' values which came from 'getPos'
-- with the current input.
setPos :: Pos -> ParserT st e ()
setPos s = ParserT \fp eob _ st -> OK# st () (posToAddr# eob s)
{-# inline setPos #-}

-- | Return the consumed span of a parser.
spanOf :: ParserT st e a -> ParserT st e Span
spanOf (ParserT f) = ParserT \fp eob s st -> case f fp eob s st of
  OK# st' a s' -> OK# st' (Span (addrToPos# eob s) (addrToPos# eob s')) s'
  x            -> unsafeCoerce# x
{-# inline spanOf #-}

-- | Bind the result together with the span of the result. CPS'd version of `spanOf`
--   for better unboxing.
withSpan :: ParserT st e a -> (a -> Span -> ParserT st e b) -> ParserT st e b
withSpan (ParserT f) g = ParserT \fp eob s st -> case f fp eob s st of
  OK# st' a s' -> runParserT# (g a (Span (addrToPos# eob s) (addrToPos# eob s'))) fp eob s' st'
  x            -> unsafeCoerce# x
{-# inline withSpan #-}

-- | Return the `B.ByteString` consumed by a parser. Note: it's more efficient to use `spanOf` and
--   `withSpan` instead.
byteStringOf :: ParserT st e a -> ParserT st e B.ByteString
byteStringOf (ParserT f) = ParserT \fp eob s st -> case f fp eob s st of
  OK# st' a s' -> OK# st' (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) s'
  x        -> unsafeCoerce# x
{-# inline byteStringOf #-}

-- | CPS'd version of `byteStringOf`. Can be more efficient, because the result is more eagerly unboxed
--   by GHC. It's more efficient to use `spanOf` or `withSpan` instead.
withByteString :: ParserT st e a -> (a -> B.ByteString -> ParserT st e b) -> ParserT st e b
withByteString (ParserT f) g = ParserT \fp eob s st -> case f fp eob s st of
  OK# st' a s' -> runParserT# (g a (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s)))) fp eob s' st'
  x        -> unsafeCoerce# x
{-# inline withByteString #-}

-- | Run a parser in a given input span. The input position and the `Int` state is restored after
--   the parser is finished, so `inSpan` does not consume input and has no side effect.  Warning:
--   this operation may crash if the given span points outside the current parsing buffer. It's
--   always safe to use `inSpan` if the span comes from a previous `withSpan` or `spanOf` call on
--   the current input.
inSpan :: Span -> ParserT st e a -> ParserT st e a
inSpan (Span s eob) (ParserT f) = ParserT \fp eob' s' st ->
  case f fp (posToAddr# eob' eob) (posToAddr# eob' s) st of
    OK# st' a _ -> OK# st' a s'
    x       -> unsafeCoerce# x
{-# inline inSpan #-}

--------------------------------------------------------------------------------

-- | Create a `B.ByteString` from a `Span`. The result is invalid if the `Span` points
--   outside the current buffer, or if the `Span` start is greater than the end position.
unsafeSpanToByteString :: Span -> ParserT st e B.ByteString
unsafeSpanToByteString (Span l r) =
  lookahead (setPos l >> byteStringOf (setPos r))
{-# inline unsafeSpanToByteString #-}

--------------------------------------------------------------------------------

-- | Read a null-terminated bytestring (a C-style string).
--
-- Consumes the null terminator.
anyCString :: ParserT st e B.ByteString
anyCString = ParserT go'
  where
    go' fp eob s0 st = go 0# s0
      where
        go n# s = case eqAddr# eob s of
          1# -> Fail# st
          _  ->
            let s' = plusAddr# s 1#
#if MIN_VERSION_base(4,16,0)
            -- TODO below is a candidate for improving with ExtendedLiterals!
            in  case eqWord8# (indexWord8OffAddr# s 0#) (wordToWord8# 0##) of
#else
            in  case eqWord# (indexWord8OffAddr# s 0#) 0## of
#endif
                  1# -> OK# st (B.PS (ForeignPtr s0 fp) 0 (I# n#)) s'
                  _  -> go (n# +# 1#) s'
{-# inline anyCString #-}

-- | Read a null-terminated bytestring (a C-style string), where the bytestring
--   is known to be null-terminated somewhere in the input.
--
-- Highly unsafe. Unless you have a guarantee that the string will be null
-- terminated before the input ends, use 'anyCString' instead. Honestly, I'm not
-- sure if this is a good function to define. But here it is.
--
-- Fails on GHC versions older than 9.0, since we make use of the
-- 'cstringLength#' primop introduced in GHC 9.0, and we aren't very useful
-- without it.
--
-- Consumes the null terminator.
anyCStringUnsafe :: ParserT st e B.ByteString
{-# inline anyCStringUnsafe #-}
#if MIN_VERSION_base(4,15,0)
anyCStringUnsafe = ParserT \fp eob s st ->
  case eqAddr# eob s of
    1# -> Fail# st
    _  -> let n#  = cstringLength# s
              s'# = plusAddr# s (n# +# 1#)
           in OK# st (B.PS (ForeignPtr s fp) 0 (I# n#)) s'#
#else
anyCStringUnsafe = error "Flatparse.Basic.anyCStringUnsafe: requires GHC 9.0 / base-4.15, not available on this compiler"
#endif

-- | Read a protobuf-style varint into a positive 'Int'.
--
-- protobuf-style varints are byte-aligned. For each byte, the lower 7 bits are
-- data and the MSB indicates if there are further bytes. Once fully parsed, the
-- 7-bit payloads are concatenated and interpreted as a little-endian unsigned
-- integer.
--
-- Fails if the varint exceeds the positive 'Int' range.
--
-- Really, these are varnats. They also match with the LEB128 varint encoding.
--
-- protobuf encodes negatives in unsigned integers using zigzag encoding. See
-- the @fromZigzag@ family of functions for this functionality.
--
-- Further reading:
-- https://developers.google.com/protocol-buffers/docs/encoding#varints
anyVarintProtobuf :: ParserT st e Int
anyVarintProtobuf = ParserT \fp eob s st ->
    case Common.anyVarintProtobuf# eob s of
      (# (##) | #) -> Fail# st
      (# | (# w#, s#, bits# #) #) ->
        case bits# ># 63# of
          0# -> OK# st (I# w#) s#
          _  -> Fail# st -- overflow
{-# inline anyVarintProtobuf #-}

--------------------------------------------------------------------------------

-- | Run a parser on a `String` input. Reminder: @OverloadedStrings@ for `B.ByteString` does not
--   yield a valid UTF-8 encoding! For non-ASCII `B.ByteString` literal input, use `runParserS` or
--   `packUTF8` for testing.
runParserS :: Parser e a -> String -> Result e a
runParserS pa s = runParser pa (Common.packUTF8 s)
