{-# language UnboxedTuples #-}

module FlatParse.Basic.Parser
  (
  -- * Parser & result types
    Parser(..)
  , Res#
  , pattern OK#, pattern Err#, pattern Fail#

  -- * Primitive combinators
  , failed
  ) where

import GHC.Exts ( Addr#, unsafeCoerce# )
import GHC.ForeignPtr ( ForeignPtrContents )

import qualified Control.Applicative as Base
import Control.Monad ( MonadPlus(..) )

-- | @Parser e a@ has an error type @e@ and a return type @a@.
newtype Parser e a = Parser {runParser# :: ForeignPtrContents -> Addr# -> Addr# -> Res# e a}

instance Functor (Parser e) where
  fmap f (Parser g) = Parser \fp eob s -> case g fp eob s of
    OK# a s -> let !b = f a in OK# b s
    x       -> unsafeCoerce# x
  {-# inline fmap #-}

  (<$) a' (Parser g) = Parser \fp eob s -> case g fp eob s of
    OK# a s -> OK# a' s
    x       -> unsafeCoerce# x
  {-# inline (<$) #-}

instance Applicative (Parser e) where
  pure a = Parser \fp eob s -> OK# a s
  {-# inline pure #-}
  Parser ff <*> Parser fa = Parser \fp eob s -> case ff fp eob s of
    OK# f s -> case fa fp eob s of
      OK# a s  -> let !b = f a in OK# b s
      x        -> unsafeCoerce# x
    x -> unsafeCoerce# x
  {-# inline (<*>) #-}
  Parser fa <* Parser fb = Parser \fp eob s -> case fa fp eob s of
    OK# a s   -> case fb fp eob s of
      OK# b s -> OK# a s
      x -> unsafeCoerce# x
    x -> unsafeCoerce# x
  {-# inline (<*) #-}
  Parser fa *> Parser fb = Parser \fp eob s -> case fa fp eob s of
    OK# a s -> fb fp eob s
    x       -> unsafeCoerce# x
  {-# inline (*>) #-}

instance Monad (Parser e) where
  return = pure
  {-# inline return #-}
  Parser fa >>= f = Parser \fp eob s -> case fa fp eob s of
    OK# a s -> runParser# (f a) fp eob s
    x       -> unsafeCoerce# x
  {-# inline (>>=) #-}
  (>>) = (*>)
  {-# inline (>>) #-}

instance Base.Alternative (Parser e) where
  empty = failed
  {-# inline empty #-}

  (<|>) = (<|>)
  {-# inline (Base.<|>) #-}

  -- TODO more efficient than default? (we do want to inline, so)
  many (Parser f) = Parser go where
    go fp eob s = case f fp eob s of
      OK# a s -> case go fp eob s of
                   OK# as s -> OK# (a:as) s
                   x        -> x
      Fail#  -> OK# [] s
      Err# e -> Err# e
  {-# inline many #-}

  some p = (:) <$> p <*> Base.many p
  {-# inline some #-}

-- | The failing parser. By default, parser choice `(<|>)` arbitrarily backtracks
--   on parser failure.
failed :: Parser e a
failed = Parser \fp eob s -> Fail#
{-# inline failed #-}

infixr 6 <|>
(<|>) :: Parser e a -> Parser e a -> Parser e a
(<|>) (Parser f) (Parser g) = Parser \fp eob s ->
  case f fp eob s of
    Fail# -> g fp eob s
    x     -> x
{-# inline[1] (<|>) #-}

{-# RULES

"flatparse/reassoc-alt" forall l m r. (l <|> m) <|> r = l <|> (m <|> r)

#-}

instance MonadPlus (Parser e) where
  mzero = failed
  {-# inline mzero #-}
  mplus = (<|>)
  {-# inline mplus #-}

--------------------------------------------------------------------------------

-- | Primitive result of a parser, stored using an unboxed sum.
--
-- Possible results are given by the pattern synonyms 'OK#', 'Fail#' and
-- 'Error#' (in that order).
type Res# e a =
  (#
    (# a, Addr# #)
  | (# #)
  | (# e #)
  #)

-- | 'Res#' constructor for a successful parse. Contains return value and a
--   pointer to the rest of the input buffer.
pattern OK# :: a -> Addr# -> Res# e a
pattern OK# a s = (# (# a, s #) | | #)

-- | 'Res#' constructor for recoverable failure.
pattern Fail# :: Res# e a
pattern Fail# = (# | (# #) | #)

-- | 'Res#' constructor for errors which are by default non-recoverable.
pattern Err# :: e -> Res# e a
pattern Err# e = (# | | (# e #) #)
{-# complete OK#, Fail#, Err# #-}
