{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Parser supporting custom error types and embeddable `IO` or `ST` actions, but
no other bells and whistles.

If you need efficient indentation parsing, consider "FlatParse.Stateful" instead.
-}

module FlatParse.Basic (
  -- * Parser types
    FP.Parser.ParserT(..)
  , FP.Parser.Parser, FP.Parser.ParserIO, FP.Parser.ParserST

  -- * Running parsers
  , Result(..)
  , runParser
  , runParserUtf8
  , runParserIO
  , runParserST
  , embedParserST
  , embedParser

  -- ** Primitive result types
  , type FP.Parser.Res#
  , pattern FP.Parser.OK#, pattern FP.Parser.Err#, pattern FP.Parser.Fail#
  , type FP.Parser.ResI#

  -- * Embedding `ST` operations
  , liftST

  -- * UTF conversion
  , Common.strToUtf8
  , Common.utf8ToStr

  -- * Character predicates
  , Common.isDigit
  , Common.isLatinLetter
  , Common.isGreekLetter

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
  , (FP.Parser.<|>)
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
  , isolateToNextNull
  , FP.Base.isolate#
  , FP.Base.isolateUnsafe#
  , FP.Switch.switch
  , FP.Switch.switchWithPost
  , FP.Switch.rawSwitchWithPost
  , Control.Applicative.many
  , FP.Base.skipMany
  , Control.Applicative.some
  , FP.Base.skipSome
  , parseWithLength

  -- ** Errors and failures
  , Control.Applicative.empty
  , FP.Base.failed
  , FP.Base.try
  , FP.Base.err
  , FP.Base.withError
  , FP.Base.withAnyResult
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
  , validPos
  , posLineCols
  , mkPos

  -- ** Text
  -- *** UTF-8
  , FP.Text.char, FP.Text.string
  , FP.Text.anyChar, FP.Text.skipAnyChar
  , FP.Text.satisfy, FP.Text.skipSatisfy
  , FP.Text.fusedSatisfy, FP.Text.skipFusedSatisfy
  , FP.Text.takeLine
  , FP.Text.takeRestString
  , linesUtf8

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

-- for WORDS_BIGENDIAN
#include "MachDeps.h"

import FlatParse.Basic.Parser
import FlatParse.Basic.Base
import FlatParse.Basic.Integers
--import FlatParse.Basic.Bytes
import FlatParse.Basic.Text
--import FlatParse.Basic.Switch
import FlatParse.Basic.Addr
import FlatParse.Common.Position
import qualified FlatParse.Common.Assorted as Common
import qualified FlatParse.Common.Numbers  as Common

-- common prefix for using/exporting parsers with their submodule
import qualified FlatParse.Basic.Parser as FP.Parser
import qualified FlatParse.Basic.Base as FP.Base
import qualified FlatParse.Basic.Integers as FP.Integers
import qualified FlatParse.Basic.Bytes as FP.Bytes
import qualified FlatParse.Basic.Text as FP.Text
import qualified FlatParse.Basic.Switch as FP.Switch
import qualified FlatParse.Basic.Addr as FP.Addr

import qualified Control.Applicative
import GHC.IO (IO(..), unsafeIOToST)
import GHC.Int
import GHC.Exts
import GHC.ForeignPtr
import GHC.ST (ST(..))
import System.IO.Unsafe
import Data.Ord ( comparing )
import Data.List ( sortBy )

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B

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

-- | Run a pure parser.
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

-- | Run a pure parser on a 'String', converting it to the corresponding UTF-8 bytes.
--
-- Reminder: @OverloadedStrings@ for 'B.ByteString' does not yield a valid UTF-8
-- encoding! For non-ASCII 'B.ByteString' literal input, use this wrapper or
-- properly convert your input first.
runParserUtf8 :: Parser e a -> String -> Result e a
runParserUtf8 pa s = runParser pa (Common.strToUtf8 s)

-- | Run an `ST`-based parser.
runParserST :: ParserST s e a -> B.ByteString -> ST s (Result e a)
runParserST pst buf = unsafeIOToST (runParserIO (unsafeCoerce# pst) buf)
{-# inlinable runParserST #-}

-- | Run an `IO`-based parser.
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

-- | Run an `ST` action in a `ParserST`.
liftST :: ST s a -> ParserST s e a
liftST (ST f) = ParserT \fp eob s st -> case f st of
  (# st, a #) -> OK# st a s
{-# inline liftST #-}

-- | Run a `ParserST` inside any parser.
embedParserST :: forall e a s. (forall s. ParserST s e a) -> ParserT s e a
embedParserST f = unsafeCoerce# (f :: ParserST RealWorld e a)
{-# inline embedParserST #-}

-- | Run a pure `Parser` inside any parser.
embedParser :: forall e a s. Parser e a -> ParserT s e a
embedParser f = unsafeCoerce# f
{-# inline embedParser #-}

--------------------------------------------------------------------------------

-- | Parse a given 'B.ByteString'.
--
-- If the bytestring is statically known, consider using 'bytes' instead.
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

-- | Run a parser in a given input 'Span'.
--
-- The input position is restored after the parser is finished, so 'inSpan' does
-- not consume input and has no side effect.
--
-- Warning: this operation may crash if the given span points outside the
-- current parsing buffer. It's always safe to use 'inSpan' if the 'Span' comes
-- from a previous 'withSpan' or 'spanOf' call on the current input.
inSpan :: Span -> ParserT st e a -> ParserT st e a
inSpan (Span s eob) (ParserT f) = ParserT \fp eob' s' st ->
  case f fp (posToAddr# eob' eob) (posToAddr# eob' s) st of
    OK# st' a _ -> OK# st' a s'
    x           -> unsafeCoerce# x
{-# inline inSpan #-}

--------------------------------------------------------------------------------

-- | Run a parser, and return the result as well as the number of bytes it
--   consumed.
parseWithLength :: ParserT st e a -> ParserT st e (a, Int)
parseWithLength (ParserT f) = ParserT \fp eob s st -> do
    case f fp eob s st of
      Fail# st'      -> Fail# st'
      Err#  st' e    -> Err#  st' e
      OK#   st' a s' -> OK#   st' (a, I# (s' `minusAddr#` s)) s'
{-# inline parseWithLength #-}

-- | Create a 'B.ByteString' from a 'Span'.
--
-- The result is invalid if the 'Span' points outside the current buffer, or if
-- the 'Span' start is greater than the end position.
unsafeSpanToByteString :: Span -> ParserT st e B.ByteString
unsafeSpanToByteString (Span l r) =
  lookahead (setPos l >> byteStringOf (setPos r))
{-# inline unsafeSpanToByteString #-}

-- | Check whether a `Pos` points into a `B.ByteString`.
validPos :: B.ByteString -> Pos -> Bool
validPos str pos =
  let go = do
        start <- getPos
        pure (start <= pos && pos <= endPos)
  in  case runParser go str of
        OK b _ -> b
        _      -> error "FlatParse.Basic.validPos: got a non-OK result, impossible"
{-# inline validPos #-}

-- | Compute corresponding line and column numbers (both starting from 0) for each `Pos` in a list,
--   assuming UTF8 encoding. Throw an error on invalid positions. Note:
--   computing lines and columns may traverse the `B.ByteString`, but it
--   traverses it only once regardless of the length of the position list.
posLineCols :: B.ByteString -> [Pos] -> [(Int, Int)]
posLineCols str poss =
  let go !line !col [] = pure []
      go line col ((i, pos):poss) = do
        p <- getPos
        if pos == p then
          ((i, (line, col)):) <$> go line col poss
        else do
          c <- anyChar
          if '\n' == c then
            go (line + 1) 0 ((i, pos):poss)
          else
            go line (col + 1) ((i, pos):poss)

      sorted :: [(Int, Pos)]
      sorted = sortBy (\(_, i) (_, j) -> compare i j) (zip [0..] poss)

  in case runParser (go 0 0 sorted) str of
       OK res _ -> snd <$> sortBy (comparing fst) res
       _        -> error "FlatParse.Basic.posLineCols: invalid position"

-- | Create a `Pos` from a line and column number. Throws an error on out-of-bounds
--   line and column numbers.
mkPos :: B.ByteString -> (Int, Int) -> Pos
mkPos str (line', col') =
  let go line col | line == line' && col == col' = getPos
      go line col = (do
        c <- anyChar
        if c == '\n' then go (line + 1) 0
                     else go line (col + 1)) <|> error "FlatParse.Basic.mkPos: invalid position"
  in case runParser (go 0 0) str of
    OK res _ -> res
    _        -> error "FlatParse.Basic.mkPos: got a non-OK result, impossible"

-- | Break an UTF-8-coded `B.ByteString` to lines. Throws an error on invalid input.
--   This is mostly useful for grabbing specific source lines for displaying error
--   messages.
linesUtf8 :: B.ByteString -> [String]
linesUtf8 str =
  let go = ([] <$ eof) <|> ((:) <$> takeLine <*> go)
  in case runParser go str of
    OK ls _ -> ls
    _       -> error "FlatParse.Basic.linesUtf8: invalid input"

--------------------------------------------------------------------------------

-- | Isolate the given parser up to (excluding) the next null byte.
--
-- Like 'isolate', all isolated bytes must be consumed. The null byte is
-- consumed afterwards.
--
-- Useful for defining parsers for null-terminated data.
isolateToNextNull :: ParserT st e a -> ParserT st e a
isolateToNextNull (ParserT p) = ParserT \fp eob s st -> go fp eob s st s
  where
    goP fp sNull s0 st =
        case p fp sNull s0 st of
          OK# st' a s' ->
            case eqAddr# s' sNull of
              1# -> -- consumed up to null: skip null, return
                    OK#   st' a (sNull `plusAddr#` 1#)
              _  -> Fail# st' -- didn't consume fully up to null: fail
          x -> x

    go8 fp eob s0 st s =
        case eqAddr# eob s of
          1# -> Fail# st -- end of input, no null: fail
          _  ->
            let s' = s `plusAddr#` 1# in
#if MIN_VERSION_base(4,16,0)
            -- below may be made clearer with ExtendedLiterals (GHC 9.8)
            case eqWord8# (indexWord8OffAddr# s 0#) (wordToWord8# 0##) of
#else
            case eqWord# (indexWord8OffAddr# s 0#) 0## of
#endif
              1# -> goP fp s   s0 st    --     0x00: isolate, execute parser
              _  -> go8 fp eob s0 st s' -- not 0x00: next please!

    {- The "find first null byte" algorithms used here are adapted from
       Hacker's Delight (2012) ch.6.

    We read a word (8 bytes) at a time for efficiency. The internal algorithm
    does byte indexing, thus endianness matters. We switch between indexing
    algorithms depending on compile-time native endianness. (The code
    surrounding the indexing is endian-independent, so we do this inline).
    -}
    go fp eob s0 st s =
        let sWord = s `plusAddr#` 8# in
        case gtAddr# sWord eob of
          1# -> -- <  8 bytes of input: revert to scanning byte by byte
            -- we _could_ operate on a word and simply ensure not to use the
            -- out-of-bounds data, which would be faster, but the act of
            -- reading could probably segfault
            go8 fp eob s0 st s
          _  -> -- >= 8 bytes of input: use efficient 8-byte scanning
#if defined(WORDS_BIGENDIAN)
            -- big-endian ("L->R"): find leftmost null byte
            let !x@(I64# x#) = Common.zbytel'intermediate (I64# (indexInt64OffAddr# s 0#)) in
#else
            -- little-endian ("R->L"): find rightmost null byte
            let !x@(I64# x#) = Common.zbyter'intermediate (I64# (indexInt64OffAddr# s 0#)) in
#endif
#if MIN_VERSION_base(4,17,0)
            case eqInt64# x# (intToInt64# 0#) of
#else
            case x# ==# 0# of
#endif
              1# -> go fp eob s0 st sWord -- no 0x00 in next word
              _  -> -- 0x00 somewhere in next word
#if defined(WORDS_BIGENDIAN)
                let !(I# nullIdx#) = Common.zbytel'toIdx x in
#else
                let !(I# nullIdx#) = Common.zbyter'toIdx x in
                -- TO TEST BE ON LE: change above CPP to zbytel, uncomment below
                -- let !(I# nullIdx#) = Common.zbytel'toIdx (I# (word2Int# (byteSwap# (int2Word# x#)))) in
#endif
                let sNull = s `plusAddr#` nullIdx# in
                goP fp sNull s0 st
{-# inline isolateToNextNull #-}

-- | Read a null-terminated bytestring (a C-style string).
--
-- Consumes the null terminator.
anyCString :: ParserT st e B.ByteString
anyCString = isolateToNextNull takeRest
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
        case bits# ># (WORD_SIZE_IN_BITS# -# 1#) of
          0# -> OK# st (I# w#) s#
          _  -> Fail# st -- overflow
{-# inline anyVarintProtobuf #-}
