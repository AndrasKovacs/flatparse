module FlatParse.Basic.Position
  ( module FlatParse.Common.Position
  , module FlatParse.Basic.Position
  ) where

import FlatParse.Basic.Parser
import FlatParse.Common.Position

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import GHC.Exts
import GHC.ForeignPtr

-- | Get the current position in the input.
getPos :: Parser e Pos
getPos = Parser \fp eob s -> OK# (addrToPos# eob s) s
{-# inline getPos #-}

-- | Set the input position. Warning: this can result in crashes if the position points outside the
--   current buffer. It is always safe to `setPos` values which came from `getPos` with the current
--   input.
setPos :: Pos -> Parser e ()
setPos s = Parser \fp eob _ -> OK# () (posToAddr# eob s)
{-# inline setPos #-}

-- | The end of the input.
endPos :: Pos
endPos = Pos 0
{-# inline endPos #-}

-- | Return the consumed span of a parser.
spanOf :: Parser e a -> Parser e Span
spanOf (Parser f) = Parser \fp eob s -> case f fp eob s of
  OK# a s' -> OK# (Span (addrToPos# eob s) (addrToPos# eob s')) s'
  x        -> unsafeCoerce# x
{-# inline spanOf #-}

-- | Bind the result together with the span of the result. CPS'd version of
--   `spanOf` for better unboxing.
withSpan :: Parser e a -> (a -> Span -> Parser e r) -> Parser e r
withSpan (Parser f) g = Parser \fp eob s -> case f fp eob s of
  OK# a s' -> runParser# (g a (Span (addrToPos# eob s) (addrToPos# eob s'))) fp eob s'
  x        -> unsafeCoerce# x
{-# inline withSpan #-}

-- | Return the `B.ByteString` consumed by a parser. Note: it's more efficient
--   to use `spanOf` and `withSpan` instead.
byteStringOf :: Parser e a -> Parser e B.ByteString
byteStringOf (Parser f) = Parser \fp eob s -> case f fp eob s of
  OK# a s' -> OK# (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) s'
  x        -> unsafeCoerce# x
{-# inline byteStringOf #-}

-- | CPS'd version of `byteStringOf`. Can be more efficient, because the result
--   is more eagerly unboxed by GHC. It's more efficient to use `spanOf` or
--   `withSpan` instead.
withByteString :: Parser e a -> (a -> B.ByteString -> Parser e r) -> Parser e r
withByteString (Parser f) g = Parser \fp eob s -> case f fp eob s of
  OK# a s' -> runParser# (g a (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s)))) fp eob s'
  x        -> unsafeCoerce# x
{-# inline withByteString #-}

-- | Run a parser in a given input span. The input position and the `Int` state is restored after
--   the parser is finished, so `inSpan` does not consume input and has no side effect.  Warning:
--   this operation may crash if the given span points outside the current parsing buffer. It's
--   always safe to use `inSpan` if the span comes from a previous `withSpan` or `spanOf` call on
--   the current input.
inSpan :: Span -> Parser e a -> Parser e a
inSpan (Span s eob) (Parser f) = Parser \fp eob' s' ->
  case f fp (posToAddr# eob' eob) (posToAddr# eob' s) of
    OK# a _ -> OK# a s'
    x       -> unsafeCoerce# x
{-# inline inSpan #-}
