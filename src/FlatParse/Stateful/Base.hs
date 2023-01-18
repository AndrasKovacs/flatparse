{-# LANGUAGE UnboxedTuples #-}

-- | Basic parser building blocks.

module FlatParse.Stateful.Base where

import Prelude hiding ( take )

import FlatParse.Stateful.Parser
import qualified FlatParse.Common.Assorted as Common

import GHC.Exts
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr ( ForeignPtr(..) )
--import Control.Applicative ( (<|>) )

-- | Throw a parsing error. By default, parser choice `(<|>)` can't backtrack
--   on parser error. Use `try` to convert an error to a recoverable failure.
err :: e -> ParserT st r e a
err e = ParserT \_fp !_r _eob _s _n st -> Err# st e
{-# inline err #-}

-- | Convert a parsing error into failure.
try :: ParserT st r e a -> ParserT st r e a
try (ParserT p) = ParserT \fp !r eob s n st -> case p fp r eob s n st of
  Err# st' _ -> Fail# st'
  x          -> x
{-# inline try #-}

-- | Convert a parsing failure to a success.
fails :: ParserT st r e a -> ParserT st r e ()
fails (ParserT p) = ParserT \fp !r eob s n st ->
  case p fp r eob s n st of
    OK#   st' _ _ _ -> Fail# st'
    Fail# st'       -> OK#   st' () s n
    Err#  st' e     -> Err#  st' e
{-# inline fails #-}

-- | Convert a parsing failure to an error.
cut :: ParserT st r e a -> e -> ParserT st r e a
cut (ParserT p) e = ParserT \fp !r eob s n st -> case p fp r eob s n st of
  Fail# st' -> Err# st' e
  x         -> x
{-# inline cut #-}

-- | Run the parser, if we get a failure, throw the given error, but if we get an error, merge the
--   inner and the newly given errors using the @e -> e -> e@ function. This can be useful for
--   implementing parsing errors which may propagate hints or accummulate contextual information.
cutting :: ParserT st r e a -> e -> (e -> e -> e) -> ParserT st r e a
cutting (ParserT p) e merge = ParserT \fp !r eob s n st -> case p fp r eob s n st of
  Fail# st'    -> Err# st' e
  Err#  st' e' -> Err# st' $! merge e' e
  x            -> x
{-# inline cutting #-}

-- | Convert a parsing failure to a `()`.
optional_ :: ParserT st r e a -> ParserT st r e ()
optional_ p = (() <$ p) <|> pure ()
{-# inline optional_ #-}

-- | CPS'd version of `optional`. This is usually more efficient, since it gets
--   rid of the extra `Maybe` allocation.
withOption
    :: ParserT st r e a -> (a -> ParserT st r e ret)
    -> ParserT st r e ret -> ParserT st r e ret
withOption (ParserT p) just (ParserT nothing) = ParserT \fp !r eob s n st ->
    case p fp r eob s n st of
      OK#   st' a s n' -> runParserT# (just a) fp r eob s n' st'
      Fail# st'        -> nothing fp r eob s n st'
      Err#  st' e      -> Err# st' e
{-# inline withOption #-}

--------------------------------------------------------------------------------

-- | Succeed if the input is empty.
eof :: ParserT st r e ()
eof = ParserT \fp !r eob s n st -> case eqAddr# eob s of
  1# -> OK#   st () s n
  _  -> Fail# st
{-# inline eof #-}

-- | Save the parsing state, then run a parser, then restore the state.
lookahead :: ParserT st r e a -> ParserT st r e a
lookahead (ParserT p) = ParserT \fp !r eob s n st ->
  case p fp r eob s n st of
    OK# st' a _ n -> OK# st' a s n
    x             -> x
{-# inline lookahead #-}

-- | @isolate n p@ runs the parser @p@ isolated to the next @n@ bytes.
--   All isolated bytes must be consumed.
--
-- Throws a runtime error if given a negative integer.
isolate :: Int -> ParserT st r e a -> ParserT st r e a
isolate = Common.withIntUnwrap# isolate#
{-# inline isolate #-}

-- | @isolate# n# p@ runs the parser @p@ isolated to the next @n#@ bytes.
--   All isolated bytes must be consumed.
--
-- Throws a runtime error if given a negative integer.
isolate# :: Int# -> ParserT st r e a -> ParserT st r e a
isolate# n# p = Common.withPosInt# n# (isolateUnsafe# n# p)
{-# inline isolate# #-}

-- | @isolateUnsafe# i# p@ runs the parser @p@ isolated to the next @i#@ bytes.
--   All isolated bytes must be consumed.
--
-- Undefined behaviour if given a negative integer.
isolateUnsafe# :: Int# -> ParserT st r e ret -> ParserT st r e ret
isolateUnsafe# i# (ParserT p) =
    withEnsure# i# $ ParserT \fp !r eob s n st ->
        let s' = plusAddr# s i#
        in  case p fp r s' s n st of
              OK# st' a s'' n' ->
                case eqAddr# s' s'' of
                  1# -> OK#   st' a s'' n'
                  _  -> Fail# st'
              x -> x
{-
isolateUnsafe# n# p = ParserT \fp eob s st ->
  let s' = plusAddr# s n#
  in  case n# <=# minusAddr# eob s of
        1# -> case runParserT# p fp s' s st of
          OK#   st' a s'' -> case eqAddr# s' s'' of
            1# -> OK#   st' a s''
            _  -> Fail# st' -- isolated segment wasn't fully consumed
          Fail# st'       -> Fail# st'
          Err#  st' e     -> Err#  st' e
        _  -> Fail# st -- you tried to isolate more than we have left
-}
{-# inline isolateUnsafe# #-}

-- | An analogue of the list `foldl` function: first parse a @b@, then parse zero or more @a@-s,
--   and combine the results in a left-nested way by the @b -> a -> b@ function. Note: this is not
--   the usual `chainl` function from the parsec libraries!
chainl :: (b -> a -> b) -> ParserT st r e b -> ParserT st r e a -> ParserT st r e b
chainl f start elem = start >>= go where
  go b = do {!a <- elem; go $! f b a} <|> pure b
{-# inline chainl #-}

-- | An analogue of the list `foldr` function: parse zero or more @a@-s, terminated by a @b@, and
--   combine the results in a right-nested way using the @a -> b -> b@ function. Note: this is not
--   the usual `chainr` function from the parsec libraries!
chainr :: (a -> b -> b) -> ParserT st r e a -> ParserT st r e b -> ParserT st r e b
chainr f (ParserT elem) (ParserT end) = ParserT go where
  go fp !r eob s n st = case elem fp r eob s n st of
    OK#   st' a s' n' -> case go fp r eob s' n' st' of
      OK# st'' b s'' n'' -> let !b' = f a b in OK# st'' b' s'' n''
      x                  -> x
    Fail# st'        -> end fp r eob s n st'
    Err#  st' e      -> Err# st' e
{-# inline chainr #-}

-- | Branch on a parser: if the first argument succeeds, continue with the second, else with the third.
--   This can produce slightly more efficient code than `(<|>)`. Moreover, `ḃranch` does not
--   backtrack from the true/false cases.
branch :: ParserT st r e a -> ParserT st r e b -> ParserT st r e b -> ParserT st r e b
branch pa pt pf = ParserT \fp !r eob s n st -> case runParserT# pa fp r eob s n st of
  OK#   st' _ s n' -> runParserT# pt fp r eob s n' st'
  Fail# st'        -> runParserT# pf fp r eob s n  st'
  Err#  st' e      -> Err# st' e
{-# inline branch #-}

-- | Succeed if the first parser succeeds and the second one fails.
notFollowedBy :: ParserT st r e a -> ParserT st r e b -> ParserT st r e a
notFollowedBy p1 p2 = p1 <* fails p2
{-# inline notFollowedBy #-}

--------------------------------------------------------------------------------

-- | Assert that there are at least @n@ bytes remaining.
--
-- Undefined behaviour if given a negative integer.
ensure :: Int -> ParserT st r e ()
ensure = Common.withIntUnwrap# ensure#
{-# inline ensure #-}

-- | Assert that there are at least @n#@ bytes remaining.
--
-- Undefined behaviour if given a negative integer.
ensure# :: Int# -> ParserT st r e ()
ensure# n# = withEnsure# n# (pure ())
{-# inline ensure# #-}

-- | Assert that there are at least @n#@ bytes remaining (CPS).
--
-- Undefined behaviour if given a negative integer.
withEnsure :: Int -> ParserT st r e ret -> ParserT st r e ret
withEnsure = Common.withIntUnwrap# withEnsure#
{-# inline withEnsure #-}

-- | Assert that there is at least 1 byte remaining (CPS).
--
-- Undefined behaviour if given a negative integer.
withEnsure1 :: ParserT st r e ret -> ParserT st r e ret
withEnsure1 (ParserT p) = ParserT \fp !r eob s n st ->
    case eqAddr# eob s of
      0# -> p fp r eob s n st
      _  -> Fail# st
{-# inline withEnsure1 #-}

-- | Assert that there are at least @n#@ bytes remaining (CPS).
--
-- Undefined behaviour if given a negative integer.
withEnsure# :: Int# -> ParserT st r e ret -> ParserT st r e ret
withEnsure# n# (ParserT p) = ParserT \fp !r eob s n st ->
    case n# <=# minusAddr# eob s of
      1# -> p fp r eob s n st
      _  -> Fail# st
{-# inline withEnsure# #-}

--------------------------------------------------------------------------------

-- | Read @n@ bytes as a 'ByteString'. Fails if newer than @n@ bytes are
--   available.
--
-- Throws a runtime error if given a negative integer.
take :: Int -> ParserT st r e B.ByteString
take (I# n#) = take# n#
{-# inline take #-}

-- | Read @n#@ bytes as a 'ByteString'. Fails if newer than @n#@ bytes are
--   available.
--
-- Throws a runtime error if given a negative integer.
take# :: Int# -> ParserT st r e B.ByteString
take# n# = Common.withPosInt# n# (takeUnsafe# n#)
{-# inline take# #-}

-- | Read @i#@ bytes as a 'ByteString'. Fails if newer than @i#@ bytes are
--   available.
--
-- Undefined behaviour if given a negative integer.
takeUnsafe# :: Int# -> ParserT st r e B.ByteString
takeUnsafe# i# = ParserT \fp !r eob s n st ->
    case i# <=# minusAddr# eob s of
      1# -> OK#   st (B.PS (ForeignPtr s fp) 0 (I# i#)) (plusAddr# s i#) n
      _  -> Fail# st
{-# inline takeUnsafe# #-}

-- | Consume the rest of the input. May return the empty bytestring.
takeRest :: ParserT st r e B.ByteString
takeRest = ParserT \fp !r eob s n st ->
  let i# = minusAddr# eob s
  in  OK# st (B.PS (ForeignPtr s fp) 0 (I# i#)) eob n
{-# inline takeRest #-}

-- | Skip forward @n@ bytes. Fails if fewer than @n@ bytes are available.
--
-- Throws a runtime error if given a negative integer.
skip :: Int -> ParserT st r e ()
skip (I# n#) = skip# n#
{-# inline skip #-}

-- | Skip forward @n@ bytes. Fails if fewer than @n@ bytes are available.
--
-- Throws a runtime error if given a negative integer.
skip# :: Int# -> ParserT st r e ()
skip# n# = atSkip# n# (pure ())
{-# inline skip# #-}

-- | Go back @i#@ bytes. (Takes a positive integer.)
--
-- Extremely unsafe. Makes no checks. Almost certainly a Bad Idea.
skipBack# :: Int# -> ParserT st r e ()
skipBack# i# = ParserT \fp !r eob s n st ->
    OK# st () (plusAddr# s (negateInt# i#)) n
{-# inline skipBack# #-}

-- | Skip forward @n#@ bytes and run the given parser. Fails if fewer than @n#@
--   bytes are available.
--
-- Throws a runtime error if given a negative integer.
atSkip# :: Int# -> ParserT st r e ret -> ParserT st r e ret
atSkip# n# p = Common.withPosInt# n# (atSkipUnsafe# n# p)
{-# inline atSkip# #-}

-- | Skip forward @i#@ bytes and run the given parser. Fails if fewer than @i@
--   bytes are available.
--
-- Undefined behaviour if given a negative integer.
atSkipUnsafe# :: Int# -> ParserT st r e ret -> ParserT st r e ret
atSkipUnsafe# i# (ParserT p) =
    withEnsure# i# $ ParserT \fp !r eob s n st ->
        p fp r eob (plusAddr# s i#) n st
{-# inline atSkipUnsafe# #-}

--------------------------------------------------------------------------------

-- | Skip a parser zero or more times.
--
-- TODO identical to one from parser-combinators
skipMany :: ParserT st r e a -> ParserT st r e ()
skipMany p = go
  where go = (p *> go) <|> pure ()
{-
skipMany (ParserT f) = ParserT go where
  go fp eob s st = case f fp eob s st of
    OK#   st' a s -> go fp eob s st'
    Fail# st'     -> OK#  st' () s
    Err#  st' e   -> Err# st' e
-}
{-# inline skipMany #-}

-- | Skip a parser one or more times.
--
-- TODO identical to one from parser-combinators
skipSome :: ParserT st r e a -> ParserT st r e ()
skipSome p = p *> skipMany p
{-# inline skipSome #-}
