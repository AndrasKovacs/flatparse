{-# language UnboxedTuples #-}

-- | Basic parser building blocks.

module FlatParse.Basic.Combinators where

import FlatParse.Basic.Parser

import GHC.Exts
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr ( ForeignPtr(..) )
import Control.Applicative ( (<|>) )

-- | Throw a parsing error. By default, parser choice `(<|>)` can't backtrack
--   on parser error. Use `try` to convert an error to a recoverable failure.
err :: e -> Parser e a
err e = Parser \fp eob s -> Err# e
{-# inline err #-}

-- | Convert a parsing failure to a success.
fails :: Parser e a -> Parser e ()
fails (Parser f) = Parser \fp eob s ->
  case f fp eob s of
    OK# _ _ -> Fail#
    Fail#   -> OK# () s
    Err# e  -> Err# e
{-# inline fails #-}

-- | Convert a parsing error into failure.
try :: Parser e a -> Parser e a
try (Parser f) = Parser \fp eob s -> case f fp eob s of
  Err# _ -> Fail#
  x      -> x
{-# inline try #-}

-- | Convert a parsing failure to a `()`.
optional_ :: Parser e a -> Parser e ()
optional_ p = (() <$ p) <|> pure ()
{-# inline optional_ #-}

-- | CPS'd version of `optional`. This is usually more efficient, since it gets
--   rid of the extra `Maybe` allocation.
withOption :: Parser e a -> (a -> Parser e r) -> Parser e r -> Parser e r
withOption (Parser f) just (Parser nothing) = Parser \fp eob s -> case f fp eob s of
  OK# a s -> runParser# (just a) fp eob s
  Fail#   -> nothing fp eob s
  Err# e  -> Err# e
{-# inline withOption #-}

-- | Convert a parsing failure to an error.
cut :: Parser e a -> e -> Parser e a
cut (Parser f) e = Parser \fp eob s -> case f fp eob s of
  Fail# -> Err# e
  x     -> x
{-# inline cut #-}

-- | Run the parser, if we get a failure, throw the given error, but if we get an error, merge the
--   inner and the newly given errors using the @e -> e -> e@ function. This can be useful for
--   implementing parsing errors which may propagate hints or accummulate contextual information.
cutting :: Parser e a -> e -> (e -> e -> e) -> Parser e a
cutting (Parser f) e merge = Parser \fp eob s -> case f fp eob s of
  Fail#   -> Err# e
  Err# e' -> let !e'' = merge e' e in Err# e''
  x       -> x
{-# inline cutting #-}

--------------------------------------------------------------------------------

-- | Save the parsing state, then run a parser, then restore the state.
lookahead :: Parser e a -> Parser e a
lookahead (Parser f) = Parser \fp eob s ->
  case f fp eob s of
    OK# a _ -> OK# a s
    x       -> x
{-# inline lookahead #-}

-- | @isolate n p@ runs the parser @p@ isolated to the next @n@ bytes. All
--   isolated bytes must be consumed.
--
-- Throws a runtime error if given a negative integer.
isolate :: Int -> Parser e a -> Parser e a
isolate (I# n#) p = withPosInt# n# (\n'# -> isolateUnsafe# n'# p)
{-# inline isolate #-}

-- | @isolate n p@ runs the parser @p@ isolated to the next @n@ bytes. All
--   isolated bytes must be consumed.
--
-- Undefined behaviour if given a negative integer.
isolateUnsafe# :: Int# -> Parser e a -> Parser e a
isolateUnsafe# n# p = Parser \fp eob s ->
  let s' = plusAddr# s n#
  in  case n# <=# minusAddr# eob s of
        1# -> case runParser# p fp s' s of
          OK# a s'' -> case eqAddr# s' s'' of
            1# -> OK# a s''
            _  -> Fail# -- isolated segment wasn't fully consumed
          Fail#     -> Fail#
          Err# e    -> Err# e
        _  -> Fail# -- you tried to isolate more than we have left
{-# inline isolateUnsafe# #-}

-- | An analogue of the list `foldl` function: first parse a @b@, then parse zero or more @a@-s,
--   and combine the results in a left-nested way by the @b -> a -> b@ function. Note: this is not
--   the usual `chainl` function from the parsec libraries!
chainl :: (b -> a -> b) -> Parser e b -> Parser e a -> Parser e b
chainl f start elem = start >>= go where
  go b = do {!a <- elem; go $! f b a} <|> pure b
{-# inline chainl #-}

-- | An analogue of the list `foldr` function: parse zero or more @a@-s, terminated by a @b@, and
--   combine the results in a right-nested way using the @a -> b -> b@ function. Note: this is not
--   the usual `chainr` function from the parsec libraries!
chainr :: (a -> b -> b) -> Parser e a -> Parser e b -> Parser e b
chainr f (Parser elem) (Parser end) = Parser go where
  go fp eob s = case elem fp eob s of
    OK# a s -> case go fp eob s of
      OK# b s -> let !b' = f a b in OK# b' s
      x       -> x
    Fail# -> end fp eob s
    Err# e -> Err# e
{-# inline chainr #-}

-- | Branch on a parser: if the first argument succeeds, continue with the second, else with the third.
--   This can produce slightly more efficient code than `(<|>)`. Moreover, `ḃranch` does not
--   backtrack from the true/false cases.
branch :: Parser e a -> Parser e b -> Parser e b -> Parser e b
branch pa pt pf = Parser \fp eob s -> case runParser# pa fp eob s of
  OK# _ s -> runParser# pt fp eob s
  Fail#   -> runParser# pf fp eob s
  Err# e  -> Err# e
{-# inline branch #-}

-- | Skip a parser zero or more times.
many_ :: Parser e a -> Parser e ()
many_ (Parser f) = Parser go where
  go fp eob s = case f fp eob s of
    OK# a s -> go fp eob s
    Fail#   -> OK# () s
    Err# e  -> Err# e
{-# inline many_ #-}

-- | Skip a parser one or more times.
some_ :: Parser e a -> Parser e ()
some_ pa = pa >> many_ pa
{-# inline some_ #-}

-- | Succeed if the first parser succeeds and the second one fails.
notFollowedBy :: Parser e a -> Parser e b -> Parser e a
notFollowedBy p1 p2 = p1 <* fails p2
{-# inline notFollowedBy #-}

--------------------------------------------------------------------------------

-- | Succeed if the input is empty.
eof :: Parser e ()
eof = Parser \fp eob s -> case eqAddr# eob s of
  1# -> OK# () s
  _  -> Fail#
{-# inline eof #-}

-- | Read the given number of bytes as a 'ByteString'.
--
-- Throws a runtime error if given a negative integer.
take :: Int -> Parser e B.ByteString
take (I# n#) = take# n#
{-# inline take #-}

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

-- | Consume the rest of the input. May return the empty bytestring.
takeRest :: Parser e B.ByteString
takeRest = Parser \fp eob s ->
  let n# = minusAddr# eob s
  in  OK# (B.PS (ForeignPtr s fp) 0 (I# n#)) eob
{-# inline takeRest #-}

-- | Skip forward @n@ bytes. Fails if fewer than @n@ bytes are available.
--
-- Throws a runtime error if given a negative integer.
skip :: Int -> Parser e ()
skip (I# os#) = skip# os#
{-# inline skip #-}

-- | Skip forward @n@ bytes. Fails if fewer than @n@ bytes are available.
--
-- Throws a runtime error if given a negative integer.
skip# :: Int# -> Parser e ()
skip# os# = atSkip# os# (pure ())
{-# inline skip# #-}

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
atSkipUnsafe# os# (Parser p) = Parser \fp eob s ->
    case os# <=# minusAddr# eob s of
      1# -> p fp eob (plusAddr# s os#)
      _  -> Fail#
{-# inline atSkipUnsafe# #-}

-- | Go back @n@ bytes.
--
-- Highly unsafe. Makes no checks.
skipBack# :: Int -> Parser e ()
skipBack# (I# i) = Parser \fp eob s -> OK# () (plusAddr# s (negateInt# i))
{-# inline skipBack# #-}

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
