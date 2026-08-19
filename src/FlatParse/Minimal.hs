{-# language Strict, UnboxedTuples, ScopedTypeVariables, TypeApplications, CPP #-}
-- {-# options_ghc -ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures #-}

{-|
A stripped-down module that's mostly useful for __deserialization__.

- There's no precise error reporting and no efficient '(<|>)' operator. Internally, we only have
  parsing errors that are represented as an 'IO' exception. This exception can be thrown and caught,
  but the overheads of the exception machinery makes this ill-advised for control flow. Instead, users
  should use 'switch' to branch on tag-like input data.
- It is not possible to build 'ByteString'-s from the input during parsing.
- Since we only use 'IO' exceptions for failure, the implementation does not need to use any unboxed
  sum types. This makes the generated code smaller and faster. Also, __unboxing__ result types works
  robustly, so there's no need to use CPS-style combinators for that.
-}

module FlatParse.Minimal (
    module FlatParse.Minimal
  , ParseException(..)
  , FlatParse.Common.Position.Pos(..)
  , FlatParse.Common.Position.endPos
  , FlatParse.Common.Position.addrToPos#
  , FlatParse.Common.Position.posToAddr#
  , FlatParse.Common.Position.Span(..)
  , FlatParse.Common.Position.unsafeSlice
  , Common.strToUtf8
  , Common.utf8ToStr
  ) where

import Control.Exception
import Control.Monad.IO.Class
import GHC.Exts
import GHC.ForeignPtr
import GHC.IO (IO(..))
import Language.Haskell.TH

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B

import qualified FlatParse.Basic as FPB
import qualified FlatParse.Basic.Switch as FPB
import qualified FlatParse.Common.Assorted as Common
import qualified FlatParse.Common.Numbers as Common
import qualified FlatParse.Common.Switch as Common
import FlatParse.Common.Position
import FlatParse.Minimal.Internal

{-# inline parseError #-}
-- | Throw the designated parse error.
parseError :: Parser a
parseError = Parser \eob s st -> parseError# st

newtype Parser a = Parser# {runParser# :: Addr# -> Addr# -> State# RealWorld -> (# a, Addr#, State# RealWorld #)}

pattern Parser :: (Addr# -> Addr# -> State# RealWorld -> (# a, Addr#, State# RealWorld #))
                  -> Parser a
pattern Parser f <- Parser# f where
  Parser f = Parser# (oneShot \eob s st -> f eob s st)

#if __GLASGOW_HASKELL__ > 900
{-# inline Parser #-}
#endif
{-# complete Parser #-}

-- Catching parsing exception
----------------------------------------------------------------------------------------------------

data ParseRes# a = ParseRes# a Addr#

{-# inline parserToIO #-}
parserToIO :: Parser a
           -> (Addr# -> Addr# -> IO (ParseRes# a))
parserToIO (Parser f) eob s =
  IO \st -> case f eob s st of (# a, s, st #) -> (# st, ParseRes# a s #)

{-# inline ioToParser #-}
ioToParser :: (Addr# -> Addr# -> IO (ParseRes# a))
           -> Parser a
ioToParser f = Parser \eob s st ->
  case f eob s of IO f -> case f st of (# st, ParseRes# a s #) -> (# a, s, st #)

-- | Catch a 'ParseException' and retry the second parser. This is not efficient enough for control
--   flow purposes. One use case is in debugging: you can catch and rethrow errors at specific
--   program points where informative debug messages can be printed.
{-# inline catchParseError #-}
catchParseError :: Parser a -> Parser a -> Parser a
catchParseError f g = ioToParser \eob s ->
  catch @ParseException (parserToIO f eob s) \_ -> parserToIO g eob s

----------------------------------------------------------------------------------------------------

instance Functor Parser where
  {-# inline fmap #-}
  fmap = \f (Parser g) -> Parser \eob s st -> case g eob s st of
    (# a, s, st #) -> let b = f a in (# b, s, st #)

  {-# inline (<$) #-}
  (<$) = \a -> fmap (\_ -> a)

instance Applicative Parser where
  {-# inline pure #-}
  pure = \a -> Parser \eob s st -> (# a, s, st #)

  {-# inline (<*>) #-}
  (<*>) = \(Parser mf) (Parser ma) -> Parser \eob s st -> case mf eob s st of
    (# f, s, st #) -> case ma eob s st of
      (# a, s, st #) -> let b = f a in (# b, s, st #)

  {-# inline (<*) #-}
  (<*) = \(Parser ma) (Parser mb) -> Parser \eob s st -> case ma eob s st of
    (# !a, s, st #) -> case mb eob s st of
      (# !b, s, st #) -> (# a, s, st #)

  {-# inline (*>) #-}
  (*>) = \(Parser ma) (Parser mb) -> Parser \eob s st -> case ma eob s st of
    (# !a, s, st #) -> mb eob s st

-- | Variant of 'pure' that does not force its argument.
{-# inline pureLazy #-}
pureLazy :: a -> Parser a
pureLazy = \ a -> Parser \eob s st -> (# a, s, st #)

instance Monad Parser where
  {-# inline return #-}
  return = pure

  {-# inline (>>=) #-}
  (>>=) = \(Parser ma) f -> Parser \eob s st -> case ma eob s st of
    (# !a, s, st #) -> runParser# (f a) eob s st

  {-# inline (>>) #-}
  (>>) = (*>)

instance MonadIO Parser where
  {-# inline liftIO #-}
  liftIO = \(IO ma) -> Parser \eob s st -> case ma st of
    (# st, !a #) -> (# a, s, st #)

-- | End-of-file, else throw error.
{-# inline eof #-}
eof :: Parser ()
eof = Parser \eob s st -> case eqAddr# eob s of
  1# -> (# (), s, st #)
  _  -> parseError# st

-- | Parsing result.
data Result a
  = OK a B.ByteString -- ^ Success case with result value and the remaining input.
  | Error             -- ^ Parsing error.
  deriving Show

-- | Run a parser.
{-# inlinable runParser #-}
runParser :: Parser a -> B.ByteString -> IO (Result a)
runParser (Parser f) b@(B.PS (ForeignPtr _ fp) _ (I# len)) =
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let end = plusAddr# buf len
    (IO \st -> case f end buf st of
      (# a, s, st #) -> let offset = minusAddr# s buf
                            !rest  = B.drop (I# offset) b
                        in (# st, OK a rest  #))
    `catch` \(_ :: ParseException) -> pure Error

{-# inline runParserUtf8 #-}
-- | Run a pure parser on a 'String', converting it to the corresponding UTF-8 bytes.
--   Reminder: @OverloadedStrings@ for 'B.ByteString' does not yield a valid UTF-8
--   encoding! For non-ASCII 'B.ByteString' literal input, use this wrapper or
--   properly convert your input first.
runParserUtf8 :: Parser a -> String -> IO (Result a)
runParserUtf8 p s = runParser p (Common.strToUtf8 s)

-- | Parse a non-empty ASCII decimal digit sequence as a 'Word'.
--   Fails on overflow.
{-# inline anyAsciiDecimalWord #-}
anyAsciiDecimalWord :: Parser Word
anyAsciiDecimalWord = Parser \eob s st -> case Common.anyAsciiDecimalWord# eob s of
  (# | (# x, s #) #) -> (# W# x, s, st #)
  (# _ | #)          -> parseError# st

-- | Parse a non-empty ASCII decimal digit sequence as a positive 'Int'.
--   Fails on overflow.
{-# inline anyAsciiDecimalInt #-}
anyAsciiDecimalInt :: Parser Int
anyAsciiDecimalInt = Parser \eob s st -> case Common.anyAsciiDecimalInt# eob s of
  (# | (# x, s #) #) -> (# I# x, s, st #)
  (# _ | #)          -> parseError# st

-- | Parse a non-empty ASCII decimal digit sequence as a positive 'Integer'.
{-# inline anyAsciiDecimalInteger #-}
anyAsciiDecimalInteger :: Parser Integer
anyAsciiDecimalInteger = unsafeEmbedBasicIO $ FPB.anyAsciiDecimalInteger

{-# inline unsafeEmbedBasicIO #-}
-- | Run a @FlatParse.Basic@ parser. Warning: the parser must not return any 'ByteString'
--   constructed from the input buffer!
unsafeEmbedBasicIO :: FPB.ParserIO () a -> Parser a
unsafeEmbedBasicIO = \(FPB.ParserT f) -> Parser \eob s st ->
#if MIN_VERSION_base(4,15,0)
  case f FinalPtr eob s st of
#else
  case f undefinedFinalizer eob s st of
#endif
    (# st, (# (# !a, s #) | | #) #) -> (# a, s, st #)
    (# st, _                     #) -> parseError# st

-- | Run a @FlatParse.Minimal@ parser inside a @FlatParse.Basic@ parser.
{-# inline embedMinimal #-}
embedMinimal :: Parser a -> FPB.ParserIO () a
embedMinimal = \(Parser f) -> FPB.ParserT \_ eob s st ->
  case f eob s st of (# !a, s, st #) -> FPB.OK# st a s

{-# inline spanOf #-}
-- | Return the consumed span of a parser.
spanOf :: Parser a -> Parser Span
spanOf (Parser f) = Parser \eob s st -> case f eob s st of
  (# a, s', st' #) -> let span = Span (addrToPos# eob s) (addrToPos# eob s')
                      in (# span, s', st' #)

-- | Skip forward @n@ bytes. Errors if fewer than @n@ bytes are available.  Behavior is undefined if
--   the given number is negative.
{-# inline skip #-}
skip :: Int -> Parser ()
skip = \(I# n) -> Parser \eob s st ->
  case n <=# minusAddr# eob s of
    1# -> (# (), plusAddr# s n, st #)
    _  -> parseError# st

-- | Go back @i@ bytes in the input. Takes a positive integer.
--
-- Extremely unsafe. Makes no checks. Almost certainly a Bad Idea.
{-# inline skipBack #-}
skipBack :: Int -> Parser ()
skipBack = \(I# n) -> Parser \eob s st -> (# (), plusAddr# s (negateInt# n), st #)

----------------------------------------------------------------------------------------------------

-- | Parse a UTF-8 string literal. This is a template function, you can use it as @$(string "foo")@,
--   for example, and the splice has type @Parser e ()@.
string :: String -> Q Exp
string str = [| unsafeEmbedBasicIO $(FPB.bytes (Common.strToBytes str)) |]

-- | Parse a UTF-8 character literal. This is a template function, you can use it as
--   @$(char \'x\')@, for example, and the splice in this case has type @Parser e ()@.
char :: Char -> Q Exp
char c = string [c]

{-|
This is a template function which makes it possible to branch on a collection of string literals in
an efficient way. By using `switch`, such branching is compiled to a trie of primitive parsing
operations, which has optimized control flow, vectorized reads and grouped checking for needed input
bytes.

The syntax is slightly magical, it overloads the usual @case@ expression. An example:

@
    $(switch [| case _ of
        "foo" -> pure True
        "bar" -> pure False |])
@

The underscore is mandatory in @case _ of@. Each branch must be a string literal, but optionally
we may have a default case, like in

@
    $(switch [| case _ of
        "foo" -> pure 10
        "bar" -> pure 20
        _     -> pure 30 |])
@

All case right hand sides must be parsers with the same type. That type is also the type
of the whole 'switch' expression.

A 'switch' has longest match semantics, and the order of cases does not matter, except for
the default case, which may only appear as the last case.

If a 'switch' does not have a default case, and no case matches the input, then it throws an
error. A fallthrough to the default case does not consume any input.
-}
switch :: Q Exp -> Q Exp
switch exp = do
  (!cases, !fallback) <- Common.parseSwitch exp
  !fromMinimal <- [| embedMinimal |]
  !toMinimal   <- [| unsafeEmbedBasicIO |]
  let !cases' = (fmap . fmap) (AppE fromMinimal) cases
  let !fallback' = fmap (AppE fromMinimal) fallback
  !t <- FPB.genTrie $! FPB.genSwitchTrie' Nothing cases' fallback'
  pure $ AppE toMinimal t
