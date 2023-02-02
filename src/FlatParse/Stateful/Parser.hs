-- | Minimal parser definition.

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-} -- needed for manual ZeroBitType def (unsure why)
{-# LANGUAGE FlexibleInstances #-}

module FlatParse.Stateful.Parser
  (
  -- * Parser
    ParserT(..)
  , Parser, ParserIO, ParserST

  -- ** Result
  , type Res#
  , pattern OK#, pattern Err#, pattern Fail#

  -- *** Internal
  , type ResI#

  -- * TODO
  , (<|>)
  ) where

import FlatParse.Common.GHCExts ( Addr#, unsafeCoerce#, ZeroBitType, Int# )
import FlatParse.Common.Parser

import GHC.ForeignPtr ( ForeignPtrContents )

import qualified Control.Applicative
import Control.Monad ( MonadPlus(..) )
import Control.Monad.IO.Class ( MonadIO(..) )
import GHC.IO ( IO(IO) )

-- | @ParserT st r e a@ is a parser with a state token type @st@, a reader
--   environment @r@, an error type @e@ and a return type @a@.
--
-- See "FlatParse.Common.Parser" for a commentary on the state token types.
newtype ParserT (st :: ZeroBitType) r e a =
    ParserT { runParserT# :: ForeignPtrContents -> r -> Addr# -> Addr# -> Int# -> st -> Res# st e a }

type Parser     = ParserT PureMode
type ParserIO   = ParserT IOMode
type ParserST s = ParserT (STMode s)

-- | You may lift IO actions into a 'ParserIO'.
instance MonadIO (ParserT IOMode r e) where
  liftIO (IO a) = ParserT \fp !r eob s n rw ->
    case a rw of (# rw', a #) -> OK# rw' a s n

instance Functor (ParserT st r e) where
  fmap f (ParserT g) = ParserT \fp !r eob s n st -> case g fp r eob s n st of
    OK# st' a s n -> OK# st' (f $! a) s n
    x             -> unsafeCoerce# x
  {-# inline fmap #-}

  (<$) a' (ParserT g) = ParserT \fp !r eob s n st -> case g fp r eob s n st of
    OK# st' _a s n -> OK# st' a' s n
    x              -> unsafeCoerce# x
  {-# inline (<$) #-}

instance Applicative (ParserT st r e) where
  pure a = ParserT \_fp !_r _eob s n st -> OK# st a s n
  {-# inline pure #-}
  ParserT ff <*> ParserT fa = ParserT \fp !r eob s n st -> case ff fp r eob s n st of
    OK# st' f s n -> case fa fp r eob s n st' of
      OK# st'' a s n -> OK# st'' (f $! a) s n
      x              -> unsafeCoerce# x
    x             -> unsafeCoerce# x
  {-# inline (<*>) #-}
  ParserT fa <* ParserT fb = ParserT \fp !r eob s n st -> case fa fp r eob s n st of
    OK# st' a s n -> case fb fp r eob s n st' of
      OK# st'' _b s n -> OK# st'' a s n
      x               -> unsafeCoerce# x
    x             -> unsafeCoerce# x
  {-# inline (<*) #-}
  ParserT fa *> ParserT fb = ParserT \fp !r eob s n st -> case fa fp r eob s n st of
    OK# st' _a s n -> fb fp r eob s n st'
    x              -> unsafeCoerce# x
  {-# inline (*>) #-}

instance Monad (ParserT st r e) where
  return = pure
  {-# inline return #-}
  ParserT fa >>= f = ParserT \fp !r eob s n st -> case fa fp r eob s n st of
    OK# st' a s n -> runParserT# (f a) fp r eob s n st'
    x             -> unsafeCoerce# x
  {-# inline (>>=) #-}
  (>>) = (*>)
  {-# inline (>>) #-}

-- | By default, parser choice `(<|>)` arbitrarily backtracks on parser failure.
instance Control.Applicative.Alternative (ParserT st r e) where
  empty = ParserT \fp !r eob s n st -> Fail# st
  {-# inline empty #-}

  (<|>) = (<|>)
  {-# inline (Control.Applicative.<|>) #-}

  many (ParserT f) = ParserT go where
    go fp !r eob s n st =
        case f fp r eob s n st of
          OK# st a s n ->
            case go fp r eob s n st of
              OK# st as s n -> OK# st (a:as) s n
              x             -> x
          Fail# st'    -> OK# st [] s n
          Err# st' e   -> Err# st e
  {-# inline many #-}

  some p = (:) <$> p <*> Control.Applicative.many p
  {-# inline some #-}

infixr 6 <|>
(<|>) :: ParserT st r e a -> ParserT st r e a -> ParserT st r e a
(<|>) (ParserT f) (ParserT g) = ParserT \fp !r eob s n st ->
  case f fp r eob s n st of
    Fail# st' -> g fp r eob s n st'
    x         -> x
{-# inline[1] (<|>) #-}

{-# RULES

"flatparse/reassoc-alt" forall l m r. (l <|> m) <|> r = l <|> (m <|> r)

#-}

instance MonadPlus (ParserT st r e) where
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
    (# a, Addr#, Int# #)
  | (# #)
  | (# e #)
  #)

-- | 'Res#' constructor for a successful parse.
--   Contains the return value, a pointer to the rest of the input buffer, and
--   the next 'Int' state, plus a state token.
pattern OK# :: (st :: ZeroBitType) -> a -> Addr# -> Int# -> Res# st e a
pattern OK# st a s n = (# st, (# (# a, s, n #) | | #) #)

-- | 'Res#' constructor for recoverable failure.
--   Contains only a state token.
pattern Fail# :: (st :: ZeroBitType) -> Res# st e a
pattern Fail# st = (# st, (# | (# #) | #) #)

-- | 'Res#' constructor for errors which are by default non-recoverable.
--    Contains the error, plus a state token.
pattern Err# :: (st :: ZeroBitType) -> e -> Res# st e a
pattern Err# st e = (# st, (# | | (# e #) #) #)
{-# complete OK#, Fail#, Err# #-}
