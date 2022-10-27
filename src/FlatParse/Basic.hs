{-# language UnboxedTuples #-}

{-|
This module implements a `Parser` supporting custom error types.  If you need
efficient indentation parsing, use "FlatParse.Stateful" instead.
-}

module FlatParse.Basic (

  -- * Parser monad
    type Parser

  -- ** Executing parsers
  , Result(..)
  , runParser

  -- * Errors and failures
  , err
  , lookahead
  , fails
  , try
  , Control.Applicative.optional
  , optional_
  , withOption
  , cut
  , cutting

  -- * Combinators
  , (Control.Applicative.<|>)
  , Control.Applicative.empty
  , branch
  , chainl
  , chainr
  , Control.Applicative.many
  , many_
  , Control.Applicative.some
  , some_
  , notFollowedBy
  , isolate
  , isolateUnsafe#

  -- * Primitive parsers
  , eof
  , switch
  , switchWithPost
  , rawSwitchWithPost

  -- ** Byte-wise
  , take
  , take#
  , takeRest
  , skip
  , atSkip#
  , skipBack#
  , getBytesOf
  , getByteStringOf
  , getCString
  , getCStringUnsafe

  -- ** Machine integers
  , module FlatParse.Basic.Integers

  -- ** 'Char', 'String'
  , getCharOf
  , getStringOf
  , getChar
  , getChar_
  , getCharASCII
  , getCharASCII_
  , getAsciiDecimalInt
  , getAsciiDecimalInteger
  , getAsciiHexInt
  , Common.isDigit
  , Common.isGreekLetter
  , Common.isLatinLetter
  , satisfy
  , satisfy_
  , satisfyASCII
  , satisfyASCII_
  , fusedSatisfy
  , fusedSatisfy_

  -- ** Positions and spans
  , module FlatParse.Basic.Position

  -- ** Location & address primitives
  , module FlatParse.Basic.Addr

  ) where

import Prelude hiding ( take, getChar )

import qualified FlatParse.Common.Assorted as Common
import FlatParse.Common.Position

import FlatParse.Basic.Parser
import FlatParse.Basic.Integers
import FlatParse.Basic.Combinators
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
