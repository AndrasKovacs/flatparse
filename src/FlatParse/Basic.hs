{-# language UnboxedTuples #-}

{-|
This module implements a `Parser` supporting custom error types.  If you need
efficient indentation parsing, use "FlatParse.Stateful" instead.

The following naming conventions are followed:

  * @withX@ are continuation passing style (CPS) parsers.
    * These are sprinkled throughout to enable better reasoning about
      performance.
  * @getX@ are regular monadic parsers.
  * @getXOf@ parse and check equality with a provided value.
  * Definitions ending with @#@ are called with unboxed values.
  * Definitions ending with @Unsafe@ are unsafe. Read their documentation before
    using.
-}

module FlatParse.Basic
  (
  -- * Parser monad
    type Parser

  -- ** Executing parsers
  , Result(..)
  , runParser

  -- * Parsers
  -- ** Base combinators, byte-wise
  , module FlatParse.Basic.Base
  , module FlatParse.Basic.Other

  -- ** Machine integers
  , module FlatParse.Basic.Integers

  -- ** 'Char', 'String'
  , module FlatParse.Basic.Strings

  -- ** Positions and spans
  , module FlatParse.Basic.Position

  -- ** Address primitives
  , module FlatParse.Basic.Addr

  -- ** Re-exports
  , (Control.Applicative.<|>)
  , Control.Applicative.empty
  , Control.Applicative.many
  , Control.Applicative.some
  , Control.Applicative.optional

  ) where

import Prelude hiding ( take, getChar )

import FlatParse.Basic.Parser
import FlatParse.Basic.Integers
import FlatParse.Basic.Base
import FlatParse.Basic.Other
import FlatParse.Basic.Strings
import FlatParse.Basic.Position
import FlatParse.Basic.Addr

import Control.Applicative

import GHC.Exts
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr ( ForeignPtr(..) )

-- | Higher-level boxed data type for parsing results.
data Result e a =
    OK a !(B.ByteString)  -- ^ Contains return value and unconsumed input
                          --   (possibly empty, if fully consumed).
  | Fail                  -- ^ Recoverable-by-default failure.
  | Err !e                -- ^ Unrecoverable-by-default error.
  deriving Show

instance Functor (Result e) where
  fmap f (OK a s) = let !b = f a in OK b s
  fmap f r        = unsafeCoerce# r
  {-# inline fmap #-}
  (<$) a (OK _ s) = OK a s
  (<$) _ r        = unsafeCoerce# r
  {-# inline (<$) #-}

--------------------------------------------------------------------------------

-- | Run a parser.
runParser :: Parser e a -> B.ByteString -> Result e a
runParser (Parser f) b@(B.PS (ForeignPtr _ fp) _ (I# len)) = unsafeDupablePerformIO do
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let end = plusAddr# buf len
    case f fp end buf of
      Err# e ->
        pure (Err e)
      OK# a s -> do
        let offset = minusAddr# s buf
        pure (OK a (B.drop (I# offset) b))
      Fail# ->
        pure Fail
{-# inlinable runParser #-}
