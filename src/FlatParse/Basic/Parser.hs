-- | Minimal parser definition.

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-} -- needed for manual ZeroBitType def (unsure why)
{-# LANGUAGE FlexibleInstances #-}

module FlatParse.Basic.Parser
  (
  -- * Parser
    ParserT(..)
  , Parser, ParserIO, ParserST
  , pureLazy

  -- * Result
  , type Res#
  , pattern OK#, pattern Err#, pattern Fail#

  -- ** Internal
  , type ResI#

  -- * Choice operator (defined with right associativity)
  , (<|>)
  ) where

import FlatParse.Common.GHCExts ( Addr#, unsafeCoerce#, ZeroBitType )
import FlatParse.Common.Parser

import GHC.ForeignPtr ( ForeignPtrContents )

import qualified Control.Applicative
import Control.Monad ( MonadPlus(..) )
import Control.Monad.IO.Class ( MonadIO(..) )
import GHC.IO ( IO(IO) )

-- | @ParserT st e a@ is a parser with a state token type @st@, an error type
--   @e@ and a return type @a@. The different state token types support
--   different embedded effects; see `Parser`, `ParserIO` and `ParserST` below.
newtype ParserT (st :: ZeroBitType) e a =
    ParserT { runParserT# :: ForeignPtrContents -> Addr# -> Addr# -> st -> Res# st e a }

-- | The type of pure parsers.
type Parser     = ParserT PureMode

-- | The type of parsers which can embed `IO` actions.
type ParserIO   = ParserT IOMode

-- | The type of parsers which can embed `ST` actions.
type ParserST s = ParserT (STMode s)

-- | You may lift IO actions into a 'ParserIO' using `liftIO`.
instance MonadIO (ParserIO e) where
  liftIO (IO a) = ParserT \fp eob s rw ->
    case a rw of (# rw', a #) -> OK# rw' a s
  {-# inline liftIO #-}

instance Functor (ParserT st e) where
  fmap f (ParserT g) = ParserT \fp eob s st -> case g fp eob s st of
    OK# st' a s -> let !b = f a in OK# st' b s
    x           -> unsafeCoerce# x
  {-# inline fmap #-}

  (<$) a' (ParserT g) = ParserT \fp eob s st -> case g fp eob s st of
    OK# st' _a s -> OK# st' a' s
    x           -> unsafeCoerce# x
  {-# inline (<$) #-}

instance Applicative (ParserT st e) where
  pure !a = ParserT \fp eob s st -> OK# st a s
  {-# inline pure #-}
  ParserT ff <*> ParserT fa = ParserT \fp eob s st -> case ff fp eob s st of
    OK# st' f s -> case fa fp eob s st' of
      OK# st'' a s -> let !b = f a in OK# st'' b s
      x            -> unsafeCoerce# x
    x           -> unsafeCoerce# x
  {-# inline (<*>) #-}
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

-- | Same as `pure` for `ParserT` except that it does not force the returned value.
pureLazy :: a -> ParserT st e a
pureLazy a = ParserT \fp eob s st -> OK# st a s
{-# inline pureLazy #-}

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
  empty = ParserT \fp eob s st -> Fail# st
  {-# inline empty #-}

  (<|>) = (<|>)
  {-# inline (Control.Applicative.<|>) #-}

  many (ParserT f) = ParserT go where
    go fp eob s st = case f fp eob s st of
      OK# st a s -> case go fp eob s st of
                      OK# st as s -> OK# st (a:as) s
                      x           -> x
      Fail# st  -> OK# st [] s
      Err# st e -> Err# st e
  {-# inline many #-}

  some p = (:) <$> p <*> Control.Applicative.many p
  {-# inline some #-}

infixr 6 <|>
-- | Choose between two parsers. If the first parser fails, try the second one,
--   but if the first one throws an error, propagate the error. This operation
--   can arbitrarily backtrack.
--
-- Note: this exported operator has different fixity than the same operator in
-- `Control.Applicative`. Hide this operator if you want to use the
-- `Alternative` version.
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
-- You should rarely need to manipulate values of this type directly. Use the
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
