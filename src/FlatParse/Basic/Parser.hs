-- | Minimal parser definition.

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-} -- needed for manual ZeroBitType def (unsure why)

module FlatParse.Basic.Parser
  (
  -- * Parser & result types
    ParserT(..)
  , type Res#
  , pattern OK#, pattern Err#, pattern Fail#

  -- ** Internal
  , type ResI#
  ) where

import FlatParse.Common.Parser
import FlatParse.Common.GHCExts ( Addr#, unsafeCoerce#, ZeroBitType )

import GHC.ForeignPtr ( ForeignPtrContents )

import qualified Control.Applicative
import Control.Monad ( MonadPlus(..) )

-- | @ParserT st e a@ has an error type @e@ and a return type @a@.
--
-- TODO explain state token here
newtype ParserT (st :: ZeroBitType) e a =
    ParserT { runParserT# :: ForeignPtrContents -> Addr# -> Addr# -> st -> Res# st e a }

type Parser     = ParserT PureMode
type ParserIO   = ParserT IOMode
type ParserST s = ParserT (STMode s)

instance Functor (ParserT st e) where
  fmap f (ParserT g) = ParserT \fp eob s st -> case g fp eob s st of
    OK# st' a s -> OK# st' (f $! a) s
    x           -> unsafeCoerce# x
  {-# inline fmap #-}

  (<$) a' (ParserT g) = ParserT \fp eob s st -> case g fp eob s st of
    OK# st' _a s -> OK# st' a' s
    x           -> unsafeCoerce# x
  {-# inline (<$) #-}

instance Applicative (ParserT st e) where
  pure a = ParserT \fp eob s st -> OK# st a s
  {-# inline pure #-}
  ParserT ff <*> ParserT fa = ParserT \fp eob s st -> case ff fp eob s st of
    OK# st' f s -> case fa fp eob s st' of
      OK# st'' a s -> OK# st'' (f $! a) s
      x            -> unsafeCoerce# x
    x           -> unsafeCoerce# x
  {-# inline (<*>) #-}
  -- TODO v rewrite simpler like *> ??
  ParserT fa <* ParserT fb = ParserT \fp eob s st -> case fa fp eob s st of
    OK# st' a s -> case fb fp eob s st' of
      OK# st'' _b s -> OK# st'' a s
      x             -> unsafeCoerce# x
    x           -> unsafeCoerce# x
  {-# inline (<*) #-}
  ParserT fa *> ParserT fb = ParserT \fp eob s st -> case fa fp eob s st of
    OK# st' _a s -> fb fp eob s st'
    x            -> unsafeCoerce# x
  {-# inline (*>) #-}

instance Monad (ParserT st e) where
  return = pure
  {-# inline return #-}
  ParserT fa >>= f = ParserT \fp eob s st -> case fa fp eob s st of
    OK# st' a s -> runParserT# (f a) fp eob s st'
    x           -> unsafeCoerce# x
  {-# inline (>>=) #-}
  (>>) = (*>)
  {-# inline (>>) #-}

-- | By default, parser choice `(<|>)` arbitrarily backtracks on parser failure.
instance Control.Applicative.Alternative (ParserT st e) where
  -- TODO 2023-01-13 raehik: consider redoing? strange setup
  --empty = ParserT \fp eob s st -> Fail#
  empty = failed
  {-# inline empty #-}

  (<|>) = (<|>)
  {-# inline (Control.Applicative.<|>) #-}

  -- TODO 2023-01-13 raehik: provide more efficient many, some impls?

-- | The failing parser. By default, parser choice `(<|>)` arbitrarily backtracks
--   on parser failure.
failed :: ParserT st e a
failed = ParserT \fp eob s st -> Fail# st
{-# inline failed #-}

infixr 6 <|>
(<|>) :: ParserT st e a -> ParserT st e a -> ParserT st e a
(<|>) (ParserT f) (ParserT g) = ParserT \fp eob s st ->
  case f fp eob s st of
    Fail# st' -> g fp eob s st'
    x         -> x
{-# inline[1] (<|>) #-}

{-# RULES

"flatparse/reassoc-alt" forall l m r. (l <|> m) <|> r = l <|> (m <|> r)

#-}

instance MonadPlus (ParserT st e) where
  mzero = Control.Applicative.empty
  {-# inline mzero #-}
  mplus = (<|>)
  {-# inline mplus #-}

--------------------------------------------------------------------------------

-- | Primitive parser result wrapped with a state token.
--
-- You should not manipulate values of this type directly. Use the
-- provided bidirectional pattern synonyms 'OK#', 'Fail#' and 'Err#'.
type Res# (st :: ZeroBitType) e a =
  (# st, ResI# e a #)

-- | Primitive parser result.
type ResI# e a =
  (#
    (# a, Addr# #)
  | (# #)
  | (# e #)
  #)

-- | 'Res#' constructor for a successful parse.
--   Contains the return value and a pointer to the rest of the input buffer,
--   plus a state token.
pattern OK# :: (st :: ZeroBitType) -> a -> Addr# -> Res# st e a
pattern OK# st a s = (# st, (# (# a, s #) | | #) #)

-- | 'Res#' constructor for recoverable failure.
--   Contains only a state token.
pattern Fail# :: (st :: ZeroBitType) -> Res# st e a
pattern Fail# st = (# st, (# | (# #) | #) #)

-- | 'Res#' constructor for errors which are by default non-recoverable.
--    Contains the error, plus a state token.
pattern Err# :: (st :: ZeroBitType) -> e -> Res# st e a
pattern Err# st e = (# st, (# | | (# e #) #) #)
{-# complete OK#, Fail#, Err# #-}
