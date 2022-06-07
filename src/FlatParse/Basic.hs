{-# language UnboxedTuples #-}

{-|
This module implements a `Parser` supporting custom error types.  If you need efficient indentation
parsing, use "FlatParse.Stateful" instead.

Many internals are exposed for hacking on and extending. These are generally
denoted by a @#@ hash suffix.
-}

module FlatParse.Basic (

  -- * Parser types and constructors
    type Parser(..)
  , type Res#
  , pattern OK#
  , pattern Fail#
  , pattern Err#
  , Result(..)

  -- * Running parsers
  , runParser
  , runParserS

  -- * Errors and failures
  , empty
  , err
  , lookahead
  , fails
  , try
  , optional
  , optional_
  , optioned
  , cut
  , cutting

  -- * Basic lexing and parsing
  , eof
  , takeBs
  , takeRestBs
  , skip
  , char
  , byte
  , bytes
  , string
  , switch
  , switchWithPost
  , rawSwitchWithPost
  , satisfy
  , satisfy_
  , satisfyASCII
  , satisfyASCII_
  , fusedSatisfy
  , fusedSatisfy_
  , anyWord8
  , anyWord8_
  , anyWord16
  , anyWord16_
  , anyWord32
  , anyWord32_
  , anyWord64
  , anyWord64_
  , anyWord
  , anyWord_
  , anyInt8
  , anyInt16
  , anyInt32
  , anyInt64
  , anyInt
  , anyChar
  , anyChar_
  , anyCharASCII
  , anyCharASCII_
  , FlatParse.Internal.isDigit
  , FlatParse.Internal.isGreekLetter
  , FlatParse.Internal.isLatinLetter
  , FlatParse.Basic.readInt
  , FlatParse.Basic.readInteger
  , anyCString

  -- ** Explicit-endianness machine integers
  , anyWord16le
  , anyWord16be
  , anyWord32le
  , anyWord32be
  , anyWord64le
  , anyWord64be
  , anyInt16le
  , anyInt16be
  , anyInt32le
  , anyInt32be
  , anyInt64le
  , anyInt64be

  -- * Combinators
  , (<|>)
  , branch
  , chainl
  , chainr
  , many
  , many_
  , some
  , some_
  , notFollowedBy
  , isolate

  -- * Positions and spans
  , Pos(..)
  , Span(..)
  , getPos
  , setPos
  , endPos
  , spanOf
  , spanned
  , byteStringOf
  , byteStringed
  , inSpan

  -- ** Position and span conversions
  , validPos
  , posLineCols
  , unsafeSpanToByteString
  , unsafeSlice
  , mkPos
  , FlatParse.Basic.lines

  -- * Getting the rest of the input as a 'String'
  , takeLine
  , traceLine
  , takeRest
  , traceRest

  -- * `String` conversions
  , packUTF8
  , unpackUTF8

  -- * Internal functions
  , ensureBytes#

  -- ** Unboxed arguments
  , takeBs#
  , atSkip#

  -- *** Location & address primitives
  , setBack#
  , withAddr#
  , takeBsOffAddr#
  , lookaheadFromAddr#
  , atAddr#

  -- ** Machine integer continuation parsers
  , withAnyWord8#
  , withAnyWord16#
  , withAnyWord32#
  , withAnyWord64#
  , withAnyInt8#
  , withAnyInt16#
  , withAnyInt32#
  , withAnyInt64#

  -- ** Unsafe
  , anyCStringUnsafe
  , scan8#
  , scan16#
  , scan32#
  , scan64#
  , scanAny8#
  , scanBytes#

  ) where

import Control.Monad
import Data.Foldable
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Word
import GHC.Exts
import GHC.Word
import GHC.Int
import GHC.ForeignPtr
import Language.Haskell.TH
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import qualified Data.Map.Strict as M

import FlatParse.Internal
import FlatParse.Internal.UnboxedNumerics

--------------------------------------------------------------------------------

-- | Primitive result of a parser. Possible results are given by `OK#`, `Err#` and `Fail#`
--   pattern synonyms.
type Res# e a =
  (#
    (# a, Addr# #)
  | (# #)
  | (# e #)
  #)

-- | Contains return value and a pointer to the rest of the input buffer.
pattern OK# :: a -> Addr# -> Res# e a
pattern OK# a s = (# (# a, s #) | | #)

-- | Constructor for errors which are by default non-recoverable.
pattern Err# :: e -> Res# e a
pattern Err# e = (# | | (# e #) #)

-- | Constructor for recoverable failure.
pattern Fail# :: Res# e a
pattern Fail# = (# | (# #) | #)
{-# complete OK#, Err#, Fail# #-}

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

-- | Run a parser on a `String` input. Reminder: @OverloadedStrings@ for `B.ByteString` does not
--   yield a valid UTF-8 encoding! For non-ASCII `B.ByteString` literal input, use `runParserS` or
--   `packUTF8` for testing.
runParserS :: Parser e a -> String -> Result e a
runParserS pa s = runParser pa (packUTF8 s)


--------------------------------------------------------------------------------

-- | The failing parser. By default, parser choice `(<|>)` arbitrarily backtracks
--   on parser failure.
empty :: Parser e a
empty = Parser \fp eob s -> Fail#
{-# inline empty #-}

-- | Throw a parsing error. By default, parser choice `(<|>)` can't backtrack
--   on parser error. Use `try` to convert an error to a recoverable failure.
err :: e -> Parser e a
err e = Parser \fp eob s -> Err# e
{-# inline err #-}

-- | Save the parsing state, then run a parser, then restore the state.
lookahead :: Parser e a -> Parser e a
lookahead (Parser f) = Parser \fp eob s ->
  case f fp eob s of
    OK# a _ -> OK# a s
    x       -> x
{-# inline lookahead #-}

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

-- | Convert a parsing failure to a `Maybe`. If possible, use `optioned` instead.
optional :: Parser e a -> Parser e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# inline optional #-}

-- | Convert a parsing failure to a `()`.
optional_ :: Parser e a -> Parser e ()
optional_ p = (() <$ p) <|> pure ()
{-# inline optional_ #-}

-- | CPS'd version of `optional`. This is usually more efficient, since it gets rid of the
--   extra `Maybe` allocation.
optioned :: Parser e a -> (a -> Parser e b) -> Parser e b -> Parser e b
optioned (Parser f) just (Parser nothing) = Parser \fp eob s -> case f fp eob s of
  OK# a s -> runParser# (just a) fp eob s
  Fail#   -> nothing fp eob s
  Err# e  -> Err# e
{-# inline optioned #-}

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


-- | Succeed if the input is empty.
eof :: Parser e ()
eof = Parser \fp eob s -> case eqAddr# eob s of
  1# -> OK# () s
  _  -> Fail#
{-# inline eof #-}

-- | Read the given number of bytes as a 'ByteString'.
--
-- Throws a runtime error if given a negative integer.
takeBs :: Int -> Parser e B.ByteString
takeBs (I# n#) = takeBs# n#
{-# inline takeBs #-}

-- | Consume the rest of the input. May return the empty bytestring.
takeRestBs :: Parser e B.ByteString
takeRestBs = Parser \fp eob s ->
  let n# = minusAddr# eob s
  in  OK# (B.PS (ForeignPtr s fp) 0 (I# n#)) eob
{-# inline takeRestBs #-}

-- | Skip forward @n@ bytes. Fails if fewer than @n@ bytes are available.
--
-- Throws a runtime error if given a negative integer.
skip :: Int -> Parser e ()
skip (I# os#) = atSkip# os# (pure ())
{-# inline skip #-}

-- | Parse a UTF-8 character literal. This is a template function, you can use it as
--   @$(char \'x\')@, for example, and the splice in this case has type @Parser e ()@.
char :: Char -> Q Exp
char c = string [c]

-- | Read a `Word8`.
byte :: Word8 -> Parser e ()
byte w = ensureBytes# 1 >> scan8# w
{-# inline byte #-}

-- | Read a sequence of bytes. This is a template function, you can use it as @$(bytes [3, 4, 5])@,
--   for example, and the splice has type @Parser e ()@.
bytes :: [Word] -> Q Exp
bytes bytes = do
  let !len = length bytes
  [| ensureBytes# len >> $(scanBytes# bytes) |]

-- | Parse a UTF-8 string literal. This is a template function, you can use it as @$(string "foo")@,
--   for example, and the splice has type @Parser e ()@.
string :: String -> Q Exp
string str = bytes (strToBytes str)

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
of the whole `switch` expression.

A `switch` has longest match semantics, and the order of cases does not matter, except for
the default case, which may only appear as the last case.

If a `switch` does not have a default case, and no case matches the input, then it returns with
failure, \without\ having consumed any input. A fallthrough to the default case also does not
consume any input.
-}
switch :: Q Exp -> Q Exp
switch = switchWithPost Nothing

{-|
Switch expression with an optional first argument for performing a post-processing action after
every successful branch matching, not including the default branch. For example, if we have
@ws :: Parser e ()@ for a whitespace parser, we might want to consume whitespace after matching
on any of the switch cases. For that case, we can define a "lexeme" version of `switch` as
follows.

@
  switch' :: Q Exp -> Q Exp
  switch' = switchWithPost (Just [| ws |])
@

Note that this @switch'@ function cannot be used in the same module it's defined in, because of the
stage restriction of Template Haskell.
-}
switchWithPost :: Maybe (Q Exp) -> Q Exp -> Q Exp
switchWithPost postAction exp = do
  !postAction <- sequence postAction
  (!cases, !fallback) <- parseSwitch exp
  genTrie $! genSwitchTrie' postAction cases fallback

-- | Version of `switchWithPost` without syntactic sugar. The second argument is the
--   list of cases, the third is the default case.
rawSwitchWithPost :: Maybe (Q Exp) -> [(String, Q Exp)] -> Maybe (Q Exp) -> Q Exp
rawSwitchWithPost postAction cases fallback = do
  !postAction <- sequence postAction
  !cases <- forM cases \(str, rhs) -> (str,) <$> rhs
  !fallback <- sequence fallback
  genTrie $! genSwitchTrie' postAction cases fallback

-- | Parse a UTF-8 `Char` for which a predicate holds.
satisfy :: (Char -> Bool) -> Parser e Char
satisfy f = Parser \fp eob s -> case runParser# anyChar fp eob s of
  OK# c s | f c -> OK# c s
  _             -> Fail#
{-#  inline satisfy #-}

-- | Skip a UTF-8 `Char` for which a predicate holds.
satisfy_ :: (Char -> Bool) -> Parser e ()
satisfy_ f = Parser \fp eob s -> case runParser# anyChar fp eob s of
  OK# c s | f c -> OK# () s
  _             -> Fail#
{-#  inline satisfy_ #-}

-- | Parse an ASCII `Char` for which a predicate holds. Assumption: the predicate must only return
--   `True` for ASCII-range characters. Otherwise this function might read a 128-255 range byte,
--   thereby breaking UTF-8 decoding.
satisfyASCII :: (Char -> Bool) -> Parser e Char
satisfyASCII f = Parser \fp eob s -> case eqAddr# eob s of
  1# -> Fail#
  _  -> case derefChar8# s of
    c1 | f (C# c1) -> OK# (C# c1) (plusAddr# s 1#)
       | otherwise -> Fail#
{-#  inline satisfyASCII #-}

-- | Skip an ASCII `Char` for which a predicate holds. Assumption: the predicate
--   must only return `True` for ASCII-range characters.
satisfyASCII_ :: (Char -> Bool) -> Parser e ()
satisfyASCII_ f = Parser \fp eob s -> case eqAddr# eob s of
  1# -> Fail#
  _  -> case derefChar8# s of
    c1 | f (C# c1) -> OK# () (plusAddr# s 1#)
       | otherwise -> Fail#
{-#  inline satisfyASCII_ #-}

-- | This is a variant of `satisfy` which allows more optimization. We can pick four testing
--   functions for the four cases for the possible number of bytes in the UTF-8 character. So in
--   @fusedSatisfy f1 f2 f3 f4@, if we read a one-byte character, the result is scrutinized with
--   @f1@, for two-bytes, with @f2@, and so on. This can result in dramatic lexing speedups.
--
--   For example, if we want to accept any letter, the naive solution would be to use
--   `Data.Char.isLetter`, but this accesses a large lookup table of Unicode character classes. We
--   can do better with @fusedSatisfy isLatinLetter isLetter isLetter isLetter@, since here the
--   `isLatinLetter` is inlined into the UTF-8 decoding, and it probably handles a great majority of
--   all cases without accessing the character table.
fusedSatisfy :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Parser e Char
fusedSatisfy f1 f2 f3 f4 = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# | f1 (C# c1) -> OK# (C# c1) (plusAddr# buf 1#)
         | otherwise  -> Fail#
      _  -> case eqAddr# eob (plusAddr# buf 1#) of
        1# -> Fail#
        _ -> case indexCharOffAddr# buf 1# of
          c2 -> case c1 `leChar#` '\xDF'# of
            1# ->
              let resc = C# (chr# (((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#) `orI#`
                                   (ord# c2 -# 0x80#)))
              in case f2 resc of
                   True -> OK# resc (plusAddr# buf 2#)
                   _    -> Fail#
            _ -> case eqAddr# eob (plusAddr# buf 2#) of
              1# -> Fail#
              _  -> case indexCharOffAddr# buf 2# of
                c3 -> case c1 `leChar#` '\xEF'# of
                  1# ->
                    let resc = C# (chr# (((ord# c1 -# 0xE0#) `uncheckedIShiftL#` 12#) `orI#`
                                         ((ord# c2 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                         (ord# c3 -# 0x80#)))
                    in case f3 resc of
                         True -> OK# resc (plusAddr# buf 3#)
                         _    -> Fail#
                  _ -> case eqAddr# eob (plusAddr# buf 3#) of
                    1# -> Fail#
                    _  -> case indexCharOffAddr# buf 3# of
                      c4 ->
                        let resc = C# (chr# (((ord# c1 -# 0xF0#) `uncheckedIShiftL#` 18#) `orI#`
                                             ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 12#) `orI#`
                                             ((ord# c3 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                              (ord# c4 -# 0x80#)))
                        in case f4 resc of
                             True -> OK# resc (plusAddr# buf 4#)
                             _    -> Fail#
{-# inline fusedSatisfy #-}

-- | Skipping variant of `fusedSatisfy`.
fusedSatisfy_ :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Parser e ()
fusedSatisfy_ f1 f2 f3 f4 = () <$ fusedSatisfy f1 f2 f3 f4
{-# inline fusedSatisfy_ #-}

-- | Parse any UTF-8-encoded `Char`.
anyChar :: Parser e Char
anyChar = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# (C# c1) (plusAddr# buf 1#)
      _  -> case eqAddr# eob (plusAddr# buf 1#) of
        1# -> Fail#
        _ -> case indexCharOffAddr# buf 1# of
          c2 -> case c1 `leChar#` '\xDF'# of
            1# ->
              let resc = ((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#) `orI#`
                          (ord# c2 -# 0x80#)
              in OK# (C# (chr# resc)) (plusAddr# buf 2#)
            _ -> case eqAddr# eob (plusAddr# buf 2#) of
              1# -> Fail#
              _  -> case indexCharOffAddr# buf 2# of
                c3 -> case c1 `leChar#` '\xEF'# of
                  1# ->
                    let resc = ((ord# c1 -# 0xE0#) `uncheckedIShiftL#` 12#) `orI#`
                               ((ord# c2 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                (ord# c3 -# 0x80#)
                    in OK# (C# (chr# resc)) (plusAddr# buf 3#)
                  _ -> case eqAddr# eob (plusAddr# buf 3#) of
                    1# -> Fail#
                    _  -> case indexCharOffAddr# buf 3# of
                      c4 ->
                        let resc = ((ord# c1 -# 0xF0#) `uncheckedIShiftL#` 18#) `orI#`
                                   ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 12#) `orI#`
                                   ((ord# c3 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                    (ord# c4 -# 0x80#)
                        in OK# (C# (chr# resc)) (plusAddr# buf 4#)
{-# inline anyChar #-}

-- | Skip any UTF-8-encoded `Char`.
anyChar_ :: Parser e ()
anyChar_ = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# () (plusAddr# buf 1#)
      _  ->
        let buf' =
              case c1 `leChar#` '\xDF'# of
                1# -> plusAddr# buf 2#
                _  -> case c1 `leChar#` '\xEF'# of
                    1# -> plusAddr# buf 3#
                    _ ->  plusAddr# buf 4#
        in case leAddr# buf' eob of
             1# -> OK# () buf'
             _  -> Fail#
{-# inline anyChar_ #-}


-- | Parse any `Char` in the ASCII range, fail if the next input character is not in the range.
--   This is more efficient than `anyChar` if we are only working with ASCII.
anyCharASCII :: Parser e Char
anyCharASCII = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# (C# c1) (plusAddr# buf 1#)
      _  -> Fail#
{-# inline anyCharASCII #-}

-- | Skip any `Char` in the ASCII range. More efficient than `anyChar_` if we're working only with
--   ASCII.
anyCharASCII_ :: Parser e ()
anyCharASCII_ = () <$ anyCharASCII
{-# inline anyCharASCII_ #-}

-- | Read a non-negative `Int` from the input, as a non-empty digit sequence.
-- The `Int` may overflow in the result.
readInt :: Parser e Int
readInt = Parser \fp eob s -> case FlatParse.Internal.readInt eob s of
  (# (##) | #)        -> Fail#
  (# | (# n, s' #) #) -> OK# (I# n) s'
{-# inline readInt #-}

-- | Read a non-negative `Integer` from the input, as a non-empty digit
-- sequence.
readInteger :: Parser e Integer
readInteger = Parser \fp eob s -> case FlatParse.Internal.readInteger fp eob s of
  (# (##) | #)        -> Fail#
  (# | (# i, s' #) #) -> OK# i s'
{-# inline readInteger #-}

--------------------------------------------------------------------------------

-- | Choose between two parsers. If the first parser fails, try the second one, but if the first one
--   throws an error, propagate the error.
infixr 6 <|>
(<|>) :: Parser e a -> Parser e a -> Parser e a
(<|>) (Parser f) (Parser g) = Parser \fp eob s ->
  case f fp eob s of
    Fail# -> g fp eob s
    x     -> x
{-# inline (<|>) #-}

-- | Branch on a parser: if the first argument succeeds, continue with the second, else with the third.
--   This can produce slightly more efficient code than `(<|>)`. Moreover, `ḃranch` does not
--   backtrack from the true/false cases.
branch :: Parser e a -> Parser e b -> Parser e b -> Parser e b
branch pa pt pf = Parser \fp eob s -> case runParser# pa fp eob s of
  OK# _ s -> runParser# pt fp eob s
  Fail#   -> runParser# pf fp eob s
  Err# e  -> Err# e
{-# inline branch #-}

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

-- | Run a parser zero or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
many :: Parser e a -> Parser e [a]
many (Parser f) = Parser go where
  go fp eob s = case f fp eob s of
    OK# a s -> case go fp eob s of
                 OK# as s -> OK# (a:as) s
                 x        -> x
    Fail#  -> OK# [] s
    Err# e -> Err# e
{-# inline many #-}

-- | Skip a parser zero or more times.
many_ :: Parser e a -> Parser e ()
many_ (Parser f) = Parser go where
  go fp eob s = case f fp eob s of
    OK# a s -> go fp eob s
    Fail#   -> OK# () s
    Err# e  -> Err# e
{-# inline many_ #-}

-- | Run a parser one or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
some :: Parser e a -> Parser e [a]
some p = (:) <$> p <*> many p
{-# inline some #-}

-- | Skip a parser one or more times.
some_ :: Parser e a -> Parser e ()
some_ pa = pa >> many_ pa
{-# inline some_ #-}

-- | Succeed if the first parser succeeds and the second one fails.
notFollowedBy :: Parser e a -> Parser e b -> Parser e a
notFollowedBy p1 p2 = p1 <* fails p2
{-# inline notFollowedBy #-}

-- | @isolate n p@ runs the parser @p@ isolated to the next @n@ bytes. All
--   isolated bytes must be consumed.
--
-- Throws a runtime error if given a negative integer.
isolate :: Int -> Parser e a -> Parser e a
isolate (I# n#) p = Parser \fp eob s ->
  let s' = plusAddr# s n#
  in  case n# <=# minusAddr# eob s of
        1# -> case n# >=# 0# of
          1# -> case runParser# p fp s' s of
            OK# a s'' -> case eqAddr# s' s'' of
              1# -> OK# a s''
              _  -> Fail# -- isolated segment wasn't fully consumed
            Fail#     -> Fail#
            Err# e    -> Err# e
          _  -> error "FlatParse.Basic.isolate: negative integer"
        _  -> Fail# -- you tried to isolate more than we have left
{-# inline isolate #-}


--------------------------------------------------------------------------------

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

-- | Bind the result together with the span of the result. CPS'd version of `spanOf`
--   for better unboxing.
spanned :: Parser e a -> (a -> Span -> Parser e b) -> Parser e b
spanned (Parser f) g = Parser \fp eob s -> case f fp eob s of
  OK# a s' -> runParser# (g a (Span (addrToPos# eob s) (addrToPos# eob s'))) fp eob s'
  x        -> unsafeCoerce# x
{-# inline spanned #-}

-- | Return the `B.ByteString` consumed by a parser. Note: it's more efficient to use `spanOf` and
--   `spanned` instead.
byteStringOf :: Parser e a -> Parser e B.ByteString
byteStringOf (Parser f) = Parser \fp eob s -> case f fp eob s of
  OK# a s' -> OK# (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) s'
  x        -> unsafeCoerce# x
{-# inline byteStringOf #-}

-- | CPS'd version of `byteStringOf`. Can be more efficient, because the result is more eagerly unboxed
--   by GHC. It's more efficient to use `spanOf` or `spanned` instead.
byteStringed :: Parser e a -> (a -> B.ByteString -> Parser e b) -> Parser e b
byteStringed (Parser f) g = Parser \fp eob s -> case f fp eob s of
  OK# a s' -> runParser# (g a (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s)))) fp eob s'
  x        -> unsafeCoerce# x
{-# inline byteStringed #-}

-- | Run a parser in a given input span. The input position and the `Int` state is restored after
--   the parser is finished, so `inSpan` does not consume input and has no side effect.  Warning:
--   this operation may crash if the given span points outside the current parsing buffer. It's
--   always safe to use `inSpan` if the span comes from a previous `spanned` or `spanOf` call on
--   the current input.
inSpan :: Span -> Parser e a -> Parser e a
inSpan (Span s eob) (Parser f) = Parser \fp eob' s' ->
  case f fp (posToAddr# eob' eob) (posToAddr# eob' s) of
    OK# a _ -> OK# a s'
    x       -> unsafeCoerce# x
{-# inline inSpan #-}

--------------------------------------------------------------------------------

-- | Check whether a `Pos` points into a `B.ByteString`.
validPos :: B.ByteString -> Pos -> Bool
validPos str pos =
  let go = do
        start <- getPos
        pure (start <= pos && pos <= endPos)
  in case runParser go str of
    OK b _ -> b
    _      -> error "impossible"
{-# inline validPos #-}

-- | Compute corresponding line and column numbers for each `Pos` in a list. Throw an error
--   on invalid positions. Note: computing lines and columns may traverse the `B.ByteString`,
--   but it traverses it only once regardless of the length of the position list.
posLineCols :: B.ByteString -> [Pos] -> [(Int, Int)]
posLineCols str poss =
  let go !line !col [] = pure []
      go line col ((i, pos):poss) = do
        p <- getPos
        if pos == p then
          ((i, (line, col)):) <$> go line col poss
        else do
          c <- anyChar
          if '\n' == c then
            go (line + 1) 0 ((i, pos):poss)
          else
            go line (col + 1) ((i, pos):poss)

      sorted :: [(Int, Pos)]
      sorted = sortBy (comparing snd) (zip [0..] poss)

  in case runParser (go 0 0 sorted) str of
       OK res _ -> snd <$> sortBy (comparing fst) res
       _        -> error "invalid position"

-- | Create a `B.ByteString` from a `Span`. The result is invalid if the `Span` points
--   outside the current buffer, or if the `Span` start is greater than the end position.
unsafeSpanToByteString :: Span -> Parser e B.ByteString
unsafeSpanToByteString (Span l r) =
  lookahead (setPos l >> byteStringOf (setPos r))
{-# inline unsafeSpanToByteString #-}

-- | Create a `Pos` from a line and column number. Throws an error on out-of-bounds
--   line and column numbers.
mkPos :: B.ByteString -> (Int, Int) -> Pos
mkPos str (line', col') =
  let go line col | line == line' && col == col' = getPos
      go line col = (do
        c <- anyChar
        if c == '\n' then go (line + 1) 0
                     else go line (col + 1)) <|> error "mkPos: invalid position"
  in case runParser (go 0 0) str of
    OK res _ -> res
    _        -> error "impossible"

-- | Break an UTF-8-coded `B.ByteString` to lines. Throws an error on invalid input.
--   This is mostly useful for grabbing specific source lines for displaying error
--   messages.
lines :: B.ByteString -> [String]
lines str =
  let go = ([] <$ eof) <|> ((:) <$> takeLine <*> go)
  in case runParser go str of
    OK ls _ -> ls
    _       -> error "linesUTF8: invalid input"

--------------------------------------------------------------------------------

-- | Parse the rest of the current line as a `String`. Assumes UTF-8 encoding,
--   throws an error if the encoding is invalid.
takeLine :: Parser e String
takeLine =
  branch eof (pure "") do
  c <- anyChar
  case c of
    '\n' -> pure ""
    _    -> (c:) <$> takeLine

-- | Parse the rest of the current line as a `String`, but restore the parsing state.
--   Assumes UTF-8 encoding. This can be used for debugging.
traceLine :: Parser e String
traceLine = lookahead takeLine

-- | Take the rest of the input as a `String`. Assumes UTF-8 encoding.
takeRest :: Parser e String
takeRest = ((:) <$> anyChar <*> takeRest) <|> pure []

-- | Get the rest of the input as a `String`, but restore the parsing state. Assumes UTF-8 encoding.
--   This can be used for debugging.
traceRest :: Parser e String
traceRest = lookahead traceRest

--------------------------------------------------------------------------------

-- | Convert an UTF-8-coded `B.ByteString` to a `String`.
unpackUTF8 :: B.ByteString -> String
unpackUTF8 str = case runParser takeRest str of
  OK a _ -> a
  _      -> error "unpackUTF8: invalid encoding"

-- | Check that the input has at least the given number of bytes.
ensureBytes# :: Int -> Parser e ()
ensureBytes# (I# len) = Parser \fp eob s ->
  case len  <=# minusAddr# eob s of
    1# -> OK# () s
    _  -> Fail#
{-# inline ensureBytes# #-}

-- | Unsafely read a concrete byte from the input. It's not checked that the input has
--   enough bytes.
scan8# :: Word8 -> Parser e ()
scan8# (W8# c) = Parser \fp eob s ->
  case indexWord8OffAddr# s 0# of
    c' -> case eqWord8'# c c' of
      1# -> OK# () (plusAddr# s 1#)
      _  -> Fail#
{-# inline scan8# #-}

-- | Unsafely read two concrete bytes from the input. It's not checked that the input has
--   enough bytes.
scan16# :: Word16 -> Parser e ()
scan16# (W16# c) = Parser \fp eob s ->
  case indexWord16OffAddr# s 0# of
    c' -> case eqWord16'# c c' of
      1# -> OK# () (plusAddr# s 2#)
      _  -> Fail#
{-# inline scan16# #-}

-- | Unsafely read four concrete bytes from the input. It's not checked that the input has
--   enough bytes.
scan32# :: Word32 -> Parser e ()
scan32# (W32# c) = Parser \fp eob s ->
  case indexWord32OffAddr# s 0# of
    c' -> case eqWord32'# c c' of
      1# -> OK# () (plusAddr# s 4#)
      _  -> Fail#
{-# inline scan32# #-}

-- | Unsafely read eight concrete bytes from the input. It's not checked that the input has
--   enough bytes.
scan64# :: Word -> Parser e ()
scan64# (W# c) = Parser \fp eob s ->
  case indexWord64OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 8#)
      _  -> Fail#
{-# inline scan64# #-}

-- | Unsafely read and return a byte from the input. It's not checked that the input is non-empty.
scanAny8# :: Parser e Word8
scanAny8# = Parser \fp eob s -> OK# (W8# (indexWord8OffAddr# s 0#)) (plusAddr# s 1#)
{-# inline scanAny8# #-}

scanPartial64# :: Int -> Word -> Parser e ()
scanPartial64# (I# len) (W# w) = Parser \fp eob s ->
  case indexWordOffAddr# s 0# of
    w' -> case uncheckedIShiftL# (8# -# len) 3# of
      sh -> case uncheckedShiftL# w' sh of
        w' -> case uncheckedShiftRL# w' sh of
          w' -> case eqWord# w w' of
            1# -> OK# () (plusAddr# s len)
            _  -> Fail#
{-# inline scanPartial64# #-}

-- | Decrease the current input position by the given number of bytes.
setBack# :: Int -> Parser e ()
setBack# (I# i) = Parser \fp eob s ->
  OK# () (plusAddr# s (negateInt# i))
{-# inline setBack# #-}

-- | Template function, creates a @Parser e ()@ which unsafely scans a given
--   sequence of bytes.
scanBytes# :: [Word] -> Q Exp
scanBytes# bytes = do
  let !(leading, w8s) = splitBytes bytes
      !scanw8s        = go w8s where
                         go (w8:[] ) = [| scan64# w8 |]
                         go (w8:w8s) = [| scan64# w8 >> $(go w8s) |]
                         go []       = [| pure () |]
  case w8s of
    [] -> go leading
          where
            go (a:b:c:d:[]) = let !w = packBytes [a, b, c, d] in [| scan32# w |]
            go (a:b:c:d:ws) = let !w = packBytes [a, b, c, d] in [| scan32# w >> $(go ws) |]
            go (a:b:[])     = let !w = packBytes [a, b]       in [| scan16# w |]
            go (a:b:ws)     = let !w = packBytes [a, b]       in [| scan16# w >> $(go ws) |]
            go (a:[])       = [| scan8# a |]
            go []           = [| pure () |]
    _  -> case leading of

      []              -> scanw8s
      [a]             -> [| scan8# a >> $scanw8s |]
      ws@[a, b]       -> let !w = packBytes ws in [| scan16# w >> $scanw8s |]
      ws@[a, b, c, d] -> let !w = packBytes ws in [| scan32# w >> $scanw8s |]
      ws              -> let !w = packBytes ws
                             !l = length ws
                         in [| scanPartial64# l w >> $scanw8s |]


-- Switching code generation
--------------------------------------------------------------------------------

#if MIN_VERSION_base(4,15,0)
mkDoE = DoE Nothing
{-# inline mkDoE #-}
#else
mkDoE = DoE
{-# inline mkDoE #-}
#endif

genTrie :: (Map (Maybe Int) Exp, Trie' (Rule, Int, Maybe Int)) -> Q Exp
genTrie (rules, t) = do
  branches <- traverse (\e -> (,) <$> (newName "rule") <*> pure e) rules

  let ix m k = case M.lookup k m of
        Nothing -> error ("key not in map: " ++ show k)
        Just a  -> a

  let ensure :: Maybe Int -> Maybe (Q Exp)
      ensure = fmap (\n -> [| ensureBytes# n |])

      fallback :: Rule -> Int ->  Q Exp
      fallback rule 0 = pure $ VarE $ fst $ ix branches rule
      fallback rule n = [| setBack# n >> $(pure $ VarE $ fst $ ix branches rule) |]

  let go :: Trie' (Rule, Int, Maybe Int) -> Q Exp
      go = \case
        Branch' (r, n, alloc) ts
          | M.null ts -> pure $ VarE $ fst $ branches M.! r
          | otherwise -> do
              !next         <- (traverse . traverse) go (M.toList ts)
              !defaultCase  <- fallback r (n + 1)

              let cases = mkDoE $
                    [BindS (VarP (mkName "c")) (VarE 'scanAny8#),
                      NoBindS (CaseE (VarE (mkName "c"))
                         (map (\(w, t) ->
                                 Match (LitP (IntegerL (fromIntegral w)))
                                       (NormalB t)
                                       [])
                              next
                          ++ [Match WildP (NormalB defaultCase) []]))]

              case ensure alloc of
                Nothing    -> pure cases
                Just alloc -> [| branch $alloc $(pure cases) $(fallback r n) |]

        Path (r, n, alloc) ws t ->
          case ensure alloc of
            Nothing    -> [| branch $(scanBytes# ws) $(go t) $(fallback r n)|]
            Just alloc -> [| branch ($alloc >> $(scanBytes# ws)) $(go t) $(fallback r n) |]

  letE
    (map (\(x, rhs) -> valD (varP x) (normalB (pure rhs)) []) (Data.Foldable.toList branches))
    (go t)

parseSwitch :: Q Exp -> Q ([(String, Exp)], Maybe Exp)
parseSwitch exp = exp >>= \case
  CaseE (UnboundVarE _) []    -> error "switch: empty clause list"
  CaseE (UnboundVarE _) cases -> do
    (!cases, !last) <- pure (init cases, last cases)
    !cases <- forM cases \case
      Match (LitP (StringL str)) (NormalB rhs) [] -> pure (str, rhs)
      _ -> error "switch: expected a match clause on a string literal"
    (!cases, !last) <- case last of
      Match (LitP (StringL str)) (NormalB rhs) [] -> pure (cases ++ [(str, rhs)], Nothing)
      Match WildP                (NormalB rhs) [] -> pure (cases, Just rhs)
      _ -> error "switch: expected a match clause on a string literal or a wildcard"
    pure (cases, last)
  _ -> error "switch: expected a \"case _ of\" expression"

genSwitchTrie' :: Maybe Exp -> [(String, Exp)] -> Maybe Exp
              -> (Map (Maybe Int) Exp, Trie' (Rule, Int, Maybe Int))
genSwitchTrie' postAction cases fallback =

  let (!branches, !strings) = unzip do
        (!i, (!str, !rhs)) <- zip [0..] cases
        case postAction of
          Nothing    -> pure ((Just i, rhs), (i, str))
          Just !post -> pure ((Just i, (VarE '(>>)) `AppE` post `AppE` rhs), (i, str))

      !m    = M.fromList ((Nothing, maybe (VarE 'empty) id fallback) : branches)
      !trie = compileTrie strings
  in (m , trie)

--------------------------------------------------------------------------------

withAnyWord8# :: (Word8'# -> Parser e a) -> Parser e a
withAnyWord8# p = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case indexWord8OffAddr# buf 0# of
    w# -> runParser# (p w#) fp eob (plusAddr# buf 1#)
{-# inline withAnyWord8# #-}

withAnyWord16# :: (Word16'# -> Parser e a) -> Parser e a
withAnyWord16# p = Parser \fp eob buf -> case 2# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexWord16OffAddr# buf 0# of
    w# -> runParser# (p w#) fp eob (plusAddr# buf 2#)
{-# inline withAnyWord16# #-}

withAnyWord32# :: (Word32'# -> Parser e a) -> Parser e a
withAnyWord32# p = Parser \fp eob buf -> case 4# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexWord32OffAddr# buf 0# of
    w# -> runParser# (p w#) fp eob (plusAddr# buf 4#)
{-# inline withAnyWord32# #-}

withAnyWord64# :: (Word# -> Parser e a) -> Parser e a
withAnyWord64# p = Parser \fp eob buf -> case 8# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexWordOffAddr# buf 0# of
    w# -> runParser# (p w#) fp eob (plusAddr# buf 8#)
{-# inline withAnyWord64# #-}

withAnyInt8# :: (Int8'# -> Parser e a) -> Parser e a
withAnyInt8# p = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case indexInt8OffAddr# buf 0# of
    i# -> runParser# (p i#) fp eob (plusAddr# buf 1#)
{-# inline withAnyInt8# #-}

withAnyInt16# :: (Int16'# -> Parser e a) -> Parser e a
withAnyInt16# p = Parser \fp eob buf -> case 2# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexInt16OffAddr# buf 0# of
    i# -> runParser# (p i#) fp eob (plusAddr# buf 2#)
{-# inline withAnyInt16# #-}

withAnyInt32# :: (Int32'# -> Parser e a) -> Parser e a
withAnyInt32# p = Parser \fp eob buf -> case 4# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexInt32OffAddr# buf 0# of
    i# -> runParser# (p i#) fp eob (plusAddr# buf 4#)
{-# inline withAnyInt32# #-}

withAnyInt64# :: (Int# -> Parser e a) -> Parser e a
withAnyInt64# p = Parser \fp eob buf -> case 8# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexInt64OffAddr# buf 0# of
    i# -> runParser# (p i#) fp eob (plusAddr# buf 8#)
{-# inline withAnyInt64# #-}

--------------------------------------------------------------------------------

-- | Parse any 'Word8' (byte).
anyWord8 :: Parser e Word8
anyWord8 = withAnyWord8# (\w# -> pure (W8# w#))
{-# inline anyWord8 #-}

-- | Skip any 'Word8' (byte).
anyWord8_ :: Parser e ()
anyWord8_ = () <$ anyWord8
{-# inline anyWord8_ #-}

-- | Parse any 'Word16'.
anyWord16 :: Parser e Word16
anyWord16 = withAnyWord16# (\w# -> pure (W16# w#))
{-# inline anyWord16 #-}

-- | Skip any 'Word16'.
anyWord16_ :: Parser e ()
anyWord16_ = () <$ anyWord16
{-# inline anyWord16_ #-}

-- | Parse any 'Word32'.
anyWord32 :: Parser e Word32
anyWord32 = withAnyWord32# (\w# -> pure (W32# w#))
{-# inline anyWord32 #-}

-- | Skip any 'Word32'.
anyWord32_ :: Parser e ()
anyWord32_ = () <$ anyWord32
{-# inline anyWord32_ #-}

-- | Parse any 'Word64'.
anyWord64 :: Parser e Word64
anyWord64 = withAnyWord64# (\w# -> pure (W64# w#))
{-# inline anyWord64 #-}

-- | Skip any 'Word64'.
anyWord64_ :: Parser e ()
anyWord64_ = () <$ anyWord64
{-# inline anyWord64_ #-}

-- | Parse any 'Word'.
anyWord :: Parser e Word
anyWord = withAnyWord64# (\w# -> pure (W# w#))
{-# inline anyWord #-}

-- | Skip any 'Word'.
anyWord_ :: Parser e ()
anyWord_ = () <$ anyWord
{-# inline anyWord_ #-}

--------------------------------------------------------------------------------

-- | Parse any 'Int8'.
anyInt8 :: Parser e Int8
anyInt8 = withAnyInt8# (\i# -> pure (I8# i#))
{-# inline anyInt8 #-}

-- | Parse any 'Int16'.
anyInt16 :: Parser e Int16
anyInt16 = withAnyInt16# (\i# -> pure (I16# i#))
{-# inline anyInt16 #-}

-- | Parse any 'Int32'.
anyInt32 :: Parser e Int32
anyInt32 = withAnyInt32# (\i# -> pure (I32# i#))
{-# inline anyInt32 #-}

-- | Parse any 'Int64'.
anyInt64 :: Parser e Int64
anyInt64 = withAnyInt64# (\i# -> pure (I64# i#))
{-# inline anyInt64 #-}

-- | Parse any 'Int'.
anyInt :: Parser e Int
anyInt = withAnyInt64# (\i# -> pure (I# i#))
{-# inline anyInt #-}

--------------------------------------------------------------------------------

-- | Parse any 'Word16' (little-endian).
anyWord16le :: Parser e Word16
anyWord16le = anyWord16
{-# inline anyWord16le #-}

-- | Parse any 'Word16' (big-endian).
anyWord16be :: Parser e Word16
anyWord16be = withAnyWord16# (\w# -> pure (W16# (byteSwap16'# w#)))
{-# inline anyWord16be #-}

-- | Parse any 'Word32' (little-endian).
anyWord32le :: Parser e Word32
anyWord32le = anyWord32
{-# inline anyWord32le #-}

-- | Parse any 'Word32' (big-endian).
anyWord32be :: Parser e Word32
anyWord32be = withAnyWord32# (\w# -> pure (W32# (byteSwap32'# w#)))
{-# inline anyWord32be #-}

-- | Parse any 'Word64' (little-endian).
anyWord64le :: Parser e Word64
anyWord64le = anyWord64
{-# inline anyWord64le #-}

-- | Parse any 'Word64' (big-endian).
anyWord64be :: Parser e Word64
anyWord64be = withAnyWord64# (\w# -> pure (W64# (byteSwap# w#)))
{-# inline anyWord64be #-}

--------------------------------------------------------------------------------

-- | Parse any 'Int16' (little-endian).
anyInt16le :: Parser e Int16
anyInt16le = anyInt16
{-# inline anyInt16le #-}

-- | Parse any 'Int16' (big-endian).
anyInt16be :: Parser e Int16
anyInt16be = withAnyWord16# (\w# -> pure (I16# (word16ToInt16# (byteSwap16'# w#))))
{-# inline anyInt16be #-}

-- | Parse any 'Int32' (little-endian).
anyInt32le :: Parser e Int32
anyInt32le = anyInt32
{-# inline anyInt32le #-}

-- | Parse any 'Int32' (big-endian).
anyInt32be :: Parser e Int32
anyInt32be = withAnyWord32# (\w# -> pure (I32# (word32ToInt32# (byteSwap32'# w#))))
{-# inline anyInt32be #-}

-- | Parse any 'Int64' (little-endian).
anyInt64le :: Parser e Int64
anyInt64le = anyInt64
{-# inline anyInt64le #-}

-- | Parse any 'Int64' (big-endian).
anyInt64be :: Parser e Int64
anyInt64be = withAnyWord64# (\w# -> pure (I64# (word2Int# (byteSwap# w#))))
{-# inline anyInt64be #-}

--------------------------------------------------------------------------------

-- | Skip forward @n@ bytes and run the given parser. Fails if fewer than @n@
--   bytes are available.
--
-- Throws a runtime error if given a negative integer.
atSkip# :: Int# -> Parser e a -> Parser e a
atSkip# os# (Parser p) = Parser \fp eob s -> case os# <=# minusAddr# eob s of
  1# -> case os# >=# 0# of
    1# -> p fp eob (plusAddr# s os#)
    _  -> error "FlatParse.Basic.atSkip#: negative integer"
  _  -> Fail#
{-# inline atSkip# #-}

-- | Read the given number of bytes as a 'ByteString'.
--
-- Throws a runtime error if given a negative integer.
takeBs# :: Int# -> Parser e B.ByteString
takeBs# n# = Parser \fp eob s -> case n# <=# minusAddr# eob s of
  1# -> -- have to runtime check for negative values, because they cause a hang
    case n# >=# 0# of
      1# -> OK# (B.PS (ForeignPtr s fp) 0 (I# n#)) (plusAddr# s n#)
      _  -> error "FlatParse.Basic.takeBs: negative integer"
  _  -> Fail#
{-# inline takeBs# #-}

--------------------------------------------------------------------------------

-- | Run a parser, passing it the current address the parser is at.
--
-- Useful for parsing offset-based data tables. For example, you may use this to
-- save the base address to use together with various 0-indexed offsets.
withAddr# :: (Addr# -> Parser e a) -> Parser e a
withAddr# p = Parser \fp eob s -> runParser# (p s) fp eob s
{-# inline withAddr# #-}

-- | @takeBsOffAddr# addr# offset# len#@ moves to @addr#@, skips @offset#@
--   bytes, reads @len#@ bytes into a 'ByteString', and restores the original
--   address.
--
-- The 'Addr#' should be from 'withAddr#'.
--
-- Useful for parsing offset-based data tables. For example, you may use this
-- together with 'withAddr#' to jump to an offset in your input and read some
-- data.
takeBsOffAddr# :: Addr# -> Int# -> Int# -> Parser e B.ByteString
takeBsOffAddr# addr# offset# len# =
    lookaheadFromAddr# addr# $ atSkip# offset# $ takeBs# len#
{-# inline takeBsOffAddr# #-}

-- | 'lookahead', but specify the address to lookahead from.
--
-- The 'Addr#' should be from 'withAddr#'.
lookaheadFromAddr# :: Addr# -> Parser e a -> Parser e a
lookaheadFromAddr# s = lookahead . atAddr# s
{-# inline lookaheadFromAddr# #-}

-- | Run a parser at the given address.
--
-- The 'Addr#' should be from 'withAddr#'.
--
-- This is a highly internal function -- you likely want 'lookaheadFromAddr#',
-- which will reset the address after running the parser.
atAddr# :: Addr# -> Parser e a -> Parser e a
atAddr# s (Parser p) = Parser \fp eob _ -> p fp eob s
{-# inline atAddr# #-}

--------------------------------------------------------------------------------

-- | Read a null-terminated bytestring (a C-style string).
--
-- Consumes the null terminator.
anyCString :: Parser e B.ByteString
anyCString = Parser \fp eob s -> go' fp eob s
  where
    go' fp eob s0 = go 0# s0
      where
        go n# s = case eqAddr# eob s of
          1# -> Fail#
          _  ->
            let s' = plusAddr# s 1#
            -- TODO below is a candidate for improving with ExtendedLiterals!
            in  case eqWord8# (indexWord8OffAddr''# s 0#) (wordToWord8''# 0##) of
                  1# -> OK# (B.PS (ForeignPtr s0 fp) 0 (I# n#)) s'
                  _  -> go (n# +# 1#) s'
{-# inline anyCString #-}

-- | Read a null-terminated bytestring (a C-style string), where the bytestring
--   is known to be null-terminated somewhere in the input.
--
-- Highly unsafe. Unless you have a guarantee that the string will be null
-- terminated before the input ends, use 'anyCString' instead. Honestly, I'm not
-- sure if this is a good function to define. But here it is.
--
-- Fails on GHC versions older than 9.0, since we make use of the
-- 'cstringLength#' primop introduced in GHC 9.0, and we aren't very useful
-- without it.
--
-- Consumes the null terminator.
anyCStringUnsafe :: Parser e B.ByteString
{-# inline anyCStringUnsafe #-}
#if MIN_VERSION_base(4,15,0)
anyCStringUnsafe = Parser \fp eob s ->
  case eqAddr# eob s of
    1# -> Fail#
    _  -> let n#  = cstringLength# s
              s'# = plusAddr# s (n# +# 1#)
           in OK# (B.PS (ForeignPtr s fp) 0 (I# n#)) s'#
#else
anyCStringUnsafe = error "Flatparse.Basic.anyCStringUnsafe: requires GHC 9.0 / base-4.15, not available on this compiler"
#endif
