{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Parser supporting a custom reader environment, custom error types and an 'Int'
state.
-}

module FlatParse.Stateful (
  -- * Parser type
    module FP.Parser

  -- * Running parsers
  , Result(..)
  , runParser
  , runParserUtf8
  , runParserIO
  , runParserST

  -- * Parsers
  -- ** Bytewise
  , FP.Base.eof
  , FP.Base.take
  , FP.Base.take#
  , FP.Base.takeUnsafe#
  , FP.Base.takeRest
  , FP.Base.skip
  , FP.Base.skip#
  , FP.Base.skipBack
  , FP.Base.skipBack#
  , FP.Base.atSkip#
  , FP.Base.atSkipUnsafe#

  , FP.Bytes.bytes
  , FP.Bytes.bytesUnsafe
  , byteString
  , anyCString
  , anyVarintProtobuf

  -- ** Combinators
  , FP.Base.branch
  , FP.Base.notFollowedBy
  , FP.Base.chainl
  , FP.Base.chainr
  , FP.Base.lookahead
  , FP.Base.ensure
  , FP.Base.ensure#
  , FP.Base.withEnsure
  , FP.Base.withEnsure1
  , FP.Base.withEnsure#
  , FP.Base.isolate
  , FP.Base.isolate#
  , FP.Base.isolateUnsafe#
  , FP.Switch.switch
  , FP.Switch.switchWithPost
  , FP.Switch.rawSwitchWithPost

  -- *** Non-specific (TODO)
  , Control.Applicative.many
  , FP.Base.skipMany
  , Control.Applicative.some
  , FP.Base.skipSome
  , Control.Applicative.empty

  -- ** Errors and failures
  , FP.Base.failed
  , FP.Base.try
  , FP.Base.err
  , FP.Base.fails
  , FP.Base.cut
  , FP.Base.cutting
  , FP.Base.optional
  , FP.Base.optional_
  , FP.Base.withOption

  -- ** Position
  , FlatParse.Common.Position.Pos(..)
  , FlatParse.Common.Position.endPos
  , FlatParse.Common.Position.addrToPos#
  , FlatParse.Common.Position.posToAddr#
  , FlatParse.Common.Position.Span(..)
  , FlatParse.Common.Position.unsafeSlice
  , getPos
  , setPos
  , spanOf
  , withSpan
  , byteStringOf
  , withByteString
  , inSpan
  , Basic.validPos
  , Basic.posLineCols
  , Basic.mkPos

  -- ** Text
  -- *** UTF-8
  , FP.Text.char, FP.Text.string
  , FP.Text.anyChar, FP.Text.skipAnyChar
  , FP.Text.satisfy, FP.Text.skipSatisfy
  , FP.Text.fusedSatisfy, FP.Text.skipFusedSatisfy
  , FP.Text.takeLine
  , FP.Text.takeRestString
  , Basic.linesUtf8

  -- *** ASCII
  , FP.Text.anyAsciiChar, FP.Text.skipAnyAsciiChar
  , FP.Text.satisfyAscii, FP.Text.skipSatisfyAscii

  -- *** ASCII-encoded numbers
  , FP.Text.anyAsciiDecimalWord
  , FP.Text.anyAsciiDecimalInt
  , FP.Text.anyAsciiDecimalInteger
  , FP.Text.anyAsciiHexWord
  , FP.Text.anyAsciiHexInt

  -- ** Machine integers
  , module FP.Integers

  -- ** Debugging parsers
  , FP.Text.traceLine
  , FP.Text.traceRest

  -- * Unsafe
  , unsafeSpanToByteString

  -- ** IO
  , unsafeLiftIO

  -- ** Parsers
  , module FP.Addr
  , anyCStringUnsafe

  ) where


import qualified FlatParse.Basic as Basic
import FlatParse.Stateful.Parser
import FlatParse.Stateful.Base
import FlatParse.Stateful.Integers
--import FlatParse.Stateful.Bytes
--import FlatParse.Stateful.Text
--import FlatParse.Stateful.Switch
import FlatParse.Stateful.Addr
import FlatParse.Common.Position
--import FlatParse.Common.Switch
import qualified FlatParse.Common.Assorted as Common
import qualified FlatParse.Common.Numbers  as Common

-- common prefix for using/exporting parsers with their submodule
import qualified FlatParse.Stateful.Parser as FP.Parser
import qualified FlatParse.Stateful.Base as FP.Base
import qualified FlatParse.Stateful.Integers as FP.Integers
import qualified FlatParse.Stateful.Bytes as FP.Bytes
import qualified FlatParse.Stateful.Text as FP.Text
import qualified FlatParse.Stateful.Switch as FP.Switch
import qualified FlatParse.Stateful.Addr as FP.Addr

import qualified Control.Applicative
import GHC.IO (IO(..))
import GHC.Exts
import GHC.ForeignPtr
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B

-- | Higher-level boxed data type for parsing results.
data Result e a =
    OK a Int !(B.ByteString)  -- ^ Contains return value, last `Int` state, unconsumed input.
  | Fail                      -- ^ Recoverable-by-default failure.
  | Err !e                    -- ^ Unrecoverble-by-default error.
  deriving Show

instance Functor (Result e) where
  fmap f (OK a s n) = let !b = f a in OK b s n
  fmap f r          = unsafeCoerce# r
  {-# inline fmap #-}
  (<$) a (OK _ s n) = OK a s n
  (<$) _ r          = unsafeCoerce# r
  {-# inline (<$) #-}

-- | Embed an IO action in a 'ParserT'. This is slightly safer than 'unsafePerformIO' because
-- it will sequenced correctly with respect to the surrounding actions, and its execution is guaranteed.
unsafeLiftIO :: IO a -> ParserT st r e a
unsafeLiftIO io = ParserT \fp !r eob s n st ->
                   let !a = unsafePerformIO io
                   in OK# st a s n
{-# inline unsafeLiftIO #-}

--------------------------------------------------------------------------------

-- | Run a parser. The `Int` argument is the initial state.
runParser :: Parser r e a -> r -> Int -> B.ByteString -> Result e a
runParser (ParserT f) !r (I# n) b@(B.PS (ForeignPtr _ fp) _ (I# len)) = unsafeDupablePerformIO $
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let end = plusAddr# buf len
    pure case f fp r end buf n proxy# of
      OK# _st a s n' -> let offset = minusAddr# s buf
                        in OK a (I# n') (B.drop (I# offset) b)

      Err# _st e ->  Err e
      Fail# _st  ->  Fail
{-# noinline runParser #-}
-- We mark this as noinline to allow power users to safely do unsafe state token coercions.
-- Details are discussed in https://github.com/AndrasKovacs/flatparse/pull/34#issuecomment-1326999390

-- | Run a parser on a 'String', converting it to the corresponding UTF-8 bytes.
--
-- Reminder: @OverloadedStrings@ for 'B.ByteString' does not yield a valid UTF-8
-- encoding! For non-ASCII 'B.ByteString' literal input, use this
-- wrpaper or properly convert your input first.
runParserUtf8 :: Parser r e a -> r -> Int -> String -> Result e a
runParserUtf8 pa r !n s = runParser pa r n (Common.strToUtf8 s)

-- | Run an ST based parser
runParserST :: (forall s. ParserST s r e a) -> r -> Int -> B.ByteString -> Result e a
runParserST pst !r i buf = unsafeDupablePerformIO (runParserIO pst r i buf)
{-# inlinable runParserST #-}

-- | Run an IO based parser
runParserIO :: ParserIO r e a -> r -> Int -> B.ByteString -> IO (Result e a)
runParserIO (ParserT f) !r (I# n) b@(B.PS (ForeignPtr _ fp) _ (I# len)) = do
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let end = plusAddr# buf len
    IO \st -> case f fp r end buf n st of
      OK# rw' a s n' ->  let offset = minusAddr# s buf
                         in (# rw', OK a (I# n') (B.drop (I# offset) b) #)

      Err# rw' e ->  (# rw', Err e #)
      Fail# rw'  ->  (# rw', Fail #)
{-# inlinable runParserIO #-}

--------------------------------------------------------------------------------

-- | Query the `Int` state.
get :: ParserT st r e Int
get = ParserT \fp !r eob s n st -> OK# st (I# n) s n
{-# inline get #-}

-- | Write the `Int` state.
put :: Int -> ParserT st r e ()
put (I# n) = ParserT \fp !r eob s _ st -> OK# st () s n
{-# inline put #-}

-- | Modify the `Int` state.
modify :: (Int -> Int) -> ParserT st r e ()
modify f = ParserT \fp !r eob s n st ->
  case f (I# n) of
    I# n -> OK# st () s n
{-# inline modify #-}

-- | Query the environment.
ask :: ParserT st r e r
ask = ParserT \fp !r eob s n st -> OK# st r s n
{-# inline ask #-}

-- | Run a parser in a modified environment.
local :: (r -> r) -> ParserT st r e a -> ParserT st r e a
local f (ParserT g) = ParserT \fp !r eob s n st -> let !r' = f r in g fp r' eob s n st
{-# inline local #-}

--------------------------------------------------------------------------------

-- | Parse a given `B.ByteString`. If the bytestring is statically known, consider using 'bytes' instead.
byteString :: B.ByteString -> ParserT st r e ()
byteString (B.PS (ForeignPtr bs fcontent) _ (I# len)) =

  let go64 :: Addr# -> Addr# -> Addr# -> Int# -> State# RealWorld -> Res# (State# RealWorld) e ()
      go64 bs bsend s n rw =
        let bs' = plusAddr# bs 8# in
        case gtAddr# bs' bsend of
          1# -> go8 bs bsend s n rw
#if MIN_VERSION_base(4,17,0)
          _  -> case eqWord64# (indexWord64OffAddr# bs 0#) (indexWord64OffAddr# s 0#) of
#else
          _  -> case eqWord# (indexWord64OffAddr# bs 0#) (indexWord64OffAddr# s 0#) of
#endif
            1# -> go64 bs' bsend (plusAddr# s 8#) n rw
            _  -> Fail# rw

      go8 :: Addr# -> Addr# -> Addr# -> Int# -> State# RealWorld -> Res# (State# RealWorld) e ()
      go8 bs bsend s n rw = case ltAddr# bs bsend of
#if MIN_VERSION_base(4,16,0)
        1# -> case eqWord8# (indexWord8OffAddr# bs 0#) (indexWord8OffAddr# s 0#) of
#else
        1# -> case eqWord# (indexWord8OffAddr# bs 0#) (indexWord8OffAddr# s 0#) of
#endif
          1# -> go8 (plusAddr# bs 1#) bsend (plusAddr# s 1#) n rw
          _  -> Fail# rw
        _  -> OK# rw () s n

      go :: Addr# -> Addr# -> Addr# -> Int# -> State# RealWorld -> Res# (State# RealWorld) e ()
      go bs bsend s n rw = case go64 bs bsend s n rw of
        (# rw', res #) -> case touch# fcontent rw' of
          rw'' -> (# rw'', res #)

  in ParserT \fp !r eob s n st ->
      case len <=# minusAddr# eob s of
           1# -> case runRW# (go bs (plusAddr# bs len) s n) of
             (# rw, a #) -> (# st, a #)
           _  -> Fail# st
{-# inline byteString #-}

--------------------------------------------------------------------------------

-- | Get the current position in the input.
getPos :: ParserT st r e Pos
getPos = ParserT \fp !r eob s n st -> OK# st (addrToPos# eob s) s n
{-# inline getPos #-}

-- | Set the input position.
--
-- Warning: this can result in crashes if the position points outside the
-- current buffer. It is always safe to 'setPos' values which came from 'getPos'
-- with the current input.
setPos :: Pos -> ParserT st r e ()
setPos s = ParserT \fp !r eob _ n st -> OK# st () (posToAddr# eob s) n
{-# inline setPos #-}

-- | Return the consumed span of a parser. Use `withSpan` if possible for better efficiency.
spanOf :: ParserT st r e a -> ParserT st r e Span
spanOf (ParserT f) = ParserT \fp !r eob s n st -> case f fp r eob s n st of
  OK# st' a s' n -> OK# st' (Span (addrToPos# eob s) (addrToPos# eob s')) s' n
  x              -> unsafeCoerce# x
{-# inline spanOf #-}

-- | Bind the result together with the span of the result. CPS'd version of `spanOf`
--   for better unboxing.
withSpan :: ParserT st r e a -> (a -> Span -> ParserT st r e b) -> ParserT st r e b
withSpan (ParserT f) g = ParserT \fp !r eob s n st -> case f fp r eob s n st of
  OK# st' a s' n -> runParserT# (g a (Span (addrToPos# eob s) (addrToPos# eob s'))) fp r eob s' n st'
  x              -> unsafeCoerce# x
{-# inline withSpan #-}

-- | Return the `B.ByteString` consumed by a parser. Note: it's more efficient to use `spanOf` and
--   `withSpan` instead.
byteStringOf :: ParserT st r e a -> ParserT st r e B.ByteString
byteStringOf (ParserT f) = ParserT \fp !r eob s n st -> case f fp r eob s n st of
  OK# st' a s' n -> OK# st' (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) s' n
  x              -> unsafeCoerce# x
{-# inline byteStringOf #-}

-- | CPS'd version of `byteStringOf`. Can be more efficient, because the result is more eagerly unboxed
--   by GHC. It's more efficient to use `spanOf` or `withSpan` instead.
withByteString :: ParserT st r e a -> (a -> B.ByteString -> ParserT st r e b) -> ParserT st r e b
withByteString (ParserT f) g = ParserT \fp !r eob s n st -> case f fp r eob s n st of
  OK# st' a s' n -> runParserT# (g a (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s)))) fp r eob s' n st'
  x              -> unsafeCoerce# x
{-# inline withByteString #-}

-- | Run a parser in a given input 'Span'.
--
-- The input position and the parser state is restored after the parser is
-- finished, so 'inSpan' does not consume input and has no side effect.
--
-- Warning: this operation may crash if the given span points outside the
-- current parsing buffer. It's always safe to use 'inSpan' if the 'Span' comes
-- from a previous 'withSpan' or 'spanOf' call on the current input.
inSpan :: Span -> ParserT st r e a -> ParserT st r e a
inSpan (Span s eob) (ParserT f) = ParserT \fp !r eob' s' n' st ->
  case f fp r (posToAddr# eob' eob) (posToAddr# eob' s) n' st of
    OK# st' a _ _ -> OK# st' a s' n'
    x             -> unsafeCoerce# x
{-# inline inSpan #-}

--------------------------------------------------------------------------------

-- | Create a 'B.ByteString' from a 'Span'.
--
-- The result is invalid if the 'Span' points outside the current buffer, or if
-- the 'Span' start is greater than the end position.
unsafeSpanToByteString :: Span -> ParserT st r e B.ByteString
unsafeSpanToByteString (Span l r) =
  lookahead (setPos l >> byteStringOf (setPos r))
{-# inline unsafeSpanToByteString #-}

--------------------------------------------------------------------------------

-- | Read a null-terminated bytestring (a C-style string).
--
-- Consumes the null terminator.
anyCString :: ParserT st r e B.ByteString
anyCString = ParserT \fp !r eob s n st -> go' fp eob s n st
  where
    go' fp eob s0 n st = go 0# s0 n
      where
        go n# s n = case eqAddr# eob s of
          1# -> Fail# st
          _  ->
            let s' = plusAddr# s 1#
#if MIN_VERSION_base(4,16,0)
            -- TODO below is a candidate for improving with ExtendedLiterals!
            in  case eqWord8# (indexWord8OffAddr# s 0#) (wordToWord8# 0##) of
#else
            in  case eqWord# (indexWord8OffAddr# s 0#) 0## of
#endif
                  1# -> OK# st (B.PS (ForeignPtr s0 fp) 0 (I# n#)) s' n
                  _  -> go (n# +# 1#) s' n
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
anyCStringUnsafe :: ParserT st r e B.ByteString
{-# inline anyCStringUnsafe #-}
#if MIN_VERSION_base(4,15,0)
anyCStringUnsafe = ParserT \fp !r eob s n st ->
  case eqAddr# eob s of
    1# -> Fail# st
    _  -> let n#  = cstringLength# s
              s'# = plusAddr# s (n# +# 1#)
           in OK# st (B.PS (ForeignPtr s fp) 0 (I# n#)) s'# n
#else
anyCStringUnsafe = error "Flatparse.Stateful.anyCStringUnsafe: requires GHC 9.0 / base-4.15, not available on this compiler"
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
anyVarintProtobuf :: ParserT st r e Int
anyVarintProtobuf = ParserT \fp !r eob s n st ->
    case Common.anyVarintProtobuf# eob s of
      (# (##) | #) -> Fail# st
      (# | (# w#, s#, bits# #) #) ->
        case bits# ># 63# of
          0# -> OK# st (I# w#) s# n
          _  -> Fail# st -- overflow
{-# inline anyVarintProtobuf #-}
