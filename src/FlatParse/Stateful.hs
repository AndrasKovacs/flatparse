{-# language UnboxedTuples #-}

{-|
This module implements a `Parser` supporting an `Int` reader environment, custom error types, and an
`Int` state.
-}

module FlatParse.Stateful (
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

  -- * Actions on the state and the environment
  , get
  , put
  , modify
  , ask
  , local

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
  , char
  , byte
  , bytes
  , string
  , switch
  , switchWithPost
  , rawSwitchWithPost
  , satisfy
  , satisfyASCII
  , satisfyASCII_
  , fusedSatisfy
  , anyWord8
  , anyWord16
  , anyWord32
  , anyWord
  , anyChar
  , anyChar_
  , anyCharASCII
  , anyCharASCII_
  , isDigit
  , isGreekLetter
  , isLatinLetter
  , FlatParse.Stateful.readInt
  , FlatParse.Stateful.readInteger

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
  , Basic.validPos
  , Basic.posLineCols
  , Basic.unsafeSpanToByteString
  , Basic.mkPos
  , Basic.lines

  -- * Getting the rest of the input
  , takeLine
  , traceLine
  , takeRest
  , traceRest

  -- * `String` conversions
  , packUTF8
  , unpackUTF8

  -- * Internal functions
  , ensureBytes#
  , scan8#
  , scan16#
  , scan32#
  , scan64#
  , scanAny8#
  , scanBytes#
  , setBack#

  ) where

import Control.Monad
import Data.Foldable
import Data.Map (Map)
import Data.Word
import GHC.Exts
import GHC.Word
import Language.Haskell.TH
import System.IO.Unsafe
import GHC.ForeignPtr

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Map.Strict as M

import FlatParse.Internal

import qualified FlatParse.Basic as Basic

--------------------------------------------------------------------------------

-- | Primitive result of a parser. Possible results are given by `OK#`, `Err#` and `Fail#`
--   pattern synonyms.
type Res# e a =
  (#
    (# a, Addr#, Int# #)
  | (# #)
  | (# e #)
  #)

-- | Contains return value, pointer to the rest of the input buffer and the nex `Int`
--   state.
pattern OK# :: a -> Addr# -> Int# -> Res# e a
pattern OK# a s n = (# (# a, s, n #) | | #)

-- | Constructor for errors which are by default non-recoverable.
pattern Err# :: e -> Res# e a
pattern Err# e = (# | | (# e #) #)

-- | Constructor for recoverable failure.
pattern Fail# :: Res# e a
pattern Fail# = (# | (# #) | #)
{-# complete OK#, Err#, Fail# #-}

-- | @Parser e a@ has an error type @e@ and a return type @a@.
newtype Parser e a = Parser {runParser# :: ForeignPtrContents -> Int# -> Addr# -> Addr# -> Int# -> Res# e a}

instance Functor (Parser e) where
  fmap f (Parser g) = Parser \fp !r eob s n -> case g fp r eob s n of
    OK# a s n -> let !b = f a in OK# b s n
    x         -> unsafeCoerce# x
  {-# inline fmap #-}

  (<$) a' (Parser g) = Parser \fp !r eob s n -> case g fp r eob s n of
    OK# a s n -> OK# a' s n
    x         -> unsafeCoerce# x
  {-# inline (<$) #-}

instance Applicative (Parser e) where
  pure a = Parser \fp !r eob s n -> OK# a s n
  {-# inline pure #-}
  Parser ff <*> Parser fa = Parser \fp !r eob s n -> case ff fp r eob s n of
    OK# f s n -> case fa fp r eob s n of
      OK# a s n  -> let !b = f a in OK# b s n
      x          -> unsafeCoerce# x
    x -> unsafeCoerce# x
  {-# inline (<*>) #-}
  Parser fa <* Parser fb = Parser \fp !r eob s n -> case fa fp r eob s n of
    OK# a s n   -> case fb fp r eob s n of
      OK# b s n -> OK# a s n
      x -> unsafeCoerce# x
    x -> unsafeCoerce# x
  {-# inline (<*) #-}
  Parser fa *> Parser fb = Parser \fp !r eob s n -> case fa fp r eob s n of
    OK# a s n -> fb fp r eob s n
    x         -> unsafeCoerce# x
  {-# inline (*>) #-}

instance Monad (Parser e) where
  return = pure
  {-# inline return #-}
  Parser fa >>= f = Parser \fp !r eob s n -> case fa fp r eob s n of
    OK# a s n -> runParser# (f a) fp r eob s n
    x         -> unsafeCoerce# x
  {-# inline (>>=) #-}
  Parser fa >> Parser fb = Parser \fp !r eob s n -> case fa fp r eob s n of
    OK# a s n -> fb fp r eob s n
    x         -> unsafeCoerce# x
  {-# inline (>>) #-}

-- | Higher-level boxed data type for parsing results.
data Result e a =
    OK a Int !(B.ByteString)  -- ^ Contains return value, last `Int` state, unconsumed input.
  | Fail                      -- ^ Recoverable-by-default failure.
  | Err !e                    -- ^ Unrecoverble-by-default error.
  deriving Show

instance Functor (Result e) where
  fmap f (OK a s n) = let !b = f a in OK b s n
  fmap f r          = unsafeCoerce# r
  {-# inline fmap #-}
  (<$) a (OK _ s n) = OK a s n
  (<$) _ r          = unsafeCoerce# r
  {-# inline (<$) #-}

--------------------------------------------------------------------------------

-- | Run a parser. The first `Int` argument is the reader environment, while the second one is the
--   state.
runParser :: Parser e a -> Int -> Int -> B.ByteString -> Result e a
runParser (Parser f) (I# r) (I# n) b@(B.PS (ForeignPtr _ fp) _ (I# len)) = unsafeDupablePerformIO do
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let end = plusAddr# buf len
    case f fp r end buf n of
      Err# e ->
        pure (Err e)
      OK# a s n -> do
        let offset = minusAddr# s buf
        pure (OK a (I# n) (B.drop (I# offset) b))
      Fail# ->
        pure Fail
{-# inlinable runParser #-}

-- | Run a parser on a `String` input. Reminder: @OverloadedStrings@ for `B.ByteString` does not
--   yield a valid UTF-8 encoding! For non-ASCII `B.ByteString` literal input, use `runParserS` or
--   `packUTF8` for testing.
runParserS :: Parser e a -> Int -> Int -> String -> Result e a
runParserS pa r !n s = runParser pa r n (packUTF8 s)

--------------------------------------------------------------------------------

-- | Query the `Int` state.
get :: Parser e Int
get = Parser \fp !r eob s n -> OK# (I# n) s n
{-# inline get #-}

-- | Write the `Int` state.
put :: Int -> Parser e ()
put (I# n) = Parser \fp !r eob s _ -> OK# () s n
{-# inline put #-}

-- | Modify the `Int` state.
modify :: (Int -> Int) -> Parser e ()
modify f = Parser \fp !r eob s n ->
  case f (I# n) of
    I# n -> OK# () s n
{-# inline modify #-}

-- | Query the `Int` environment.
ask :: Parser e Int
ask = Parser \fp !r eob s n -> OK# (I# r) s n
{-# inline ask #-}

-- | Run a parser in a modified environment.
local :: (Int -> Int) -> Parser e a -> Parser e a
local f (Parser g) = Parser \fp !r eob s n -> let !(I# r') = f (I# r) in g fp r' eob s n
{-# inline local #-}

--------------------------------------------------------------------------------

-- | The failing parser. By default, parser choice `(<|>)` arbitrarily backtracks
--   on parser failure.
empty :: Parser e a
empty = Parser \fp !r eob s n -> Fail#
{-# inline empty #-}

-- | Throw a parsing error. By default, parser choice `(<|>)` can't backtrack
--   on parser error. Use `try` to convert an error to a recoverable failure.
err :: e -> Parser e a
err e = Parser \fp !r eob s n -> Err# e
{-# inline err #-}

-- | Save the parsing state, then run a parser, then restore the state.
lookahead :: Parser e a -> Parser e a
lookahead (Parser f) = Parser \fp !r eob s n ->
  case f fp r eob s n of
    OK# a _ _ -> OK# a s n
    x         -> x
{-# inline lookahead #-}

-- | Convert a parsing failure to a success.
fails :: Parser e a -> Parser e ()
fails (Parser f) = Parser \fp !r eob s n ->
  case f fp r eob s n of
    OK# _ _ _ -> Fail#
    Fail#     -> OK# () s n
    Err# e    -> Err# e
{-# inline fails #-}

-- | Convert a parsing error into failure.
try :: Parser e a -> Parser e a
try (Parser f) = Parser \fp !r eob s n -> case f fp r eob s n of
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
optioned (Parser f) just (Parser nothing) = Parser \fp !r eob s n -> case f fp r eob s n of
  OK# a s n -> runParser# (just a) fp r eob s n
  Fail#     -> nothing fp r eob s n
  Err# e    -> Err# e
{-# inline optioned #-}

-- | Convert a parsing failure to an error.
cut :: Parser e a -> e -> Parser e a
cut (Parser f) e = Parser \fp !r eob s n -> case f fp r eob s n of
  Fail# -> Err# e
  x     -> x
{-# inline cut #-}

-- | Run the parser, if we get a failure, throw the given error, but if we get an error, merge the
--   inner and the newly given errors using the @e -> e -> e@ function. This can be useful for
--   implementing parsing errors which may propagate hints or accummulate contextual information.
cutting :: Parser e a -> e -> (e -> e -> e) -> Parser e a
cutting (Parser f) e merge = Parser \fp !r eob s n -> case f fp r eob s n of
  Fail#   -> Err# e
  Err# e' -> let !e'' = merge e' e in Err# e''
  x       -> x
{-# inline cutting #-}

--------------------------------------------------------------------------------


-- | Succeed if the input is empty.
eof :: Parser e ()
eof = Parser \fp !r eob s n -> case eqAddr# eob s of
  1# -> OK# () s n
  _  -> Fail#
{-# inline eof #-}

-- | Parse a UTF-8 character literal. This is a template function, you can use it as
--   @$(char \'x\')@, for example, and the splice in this case has type @Parser e ()@.
char :: Char -> Q Exp
char c = string [c]

-- | Read a `Word8`.
byte :: Word8 -> Parser e ()
byte (W8# w) = ensureBytes# 1 >> scan8# (W# w)
{-# inline byte #-}

-- | Read a sequence of bytes. This is a template function, you can use it as @$(bytes [3, 4, 5])@,
--   for example, and the splice has type @Parser e ()@.
bytes :: [Word8] -> Q Exp
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
every successful branch matching. For example, if we have @ws :: Parser e ()@ for a
whitespace parser, we might want to consume whitespace after matching on any of the switch
cases. For that case, we can define a "lexeme" version of `switch` as follows.

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
satisfy f = Parser \fp !r eob s n -> case runParser# anyChar fp r eob s n of
  OK# c s n | f c -> OK# c s n
  _               -> Fail#
{-#  inline satisfy #-}

-- | Parse an ASCII `Char` for which a predicate holds. Assumption: the predicate must only return
--   `True` for ASCII-range characters. Otherwise this function might read a 128-255 range byte,
--   thereby breaking UTF-8 decoding.
satisfyASCII :: (Char -> Bool) -> Parser e Char
satisfyASCII f = Parser \fp !r eob s n -> case eqAddr# eob s of
  1# -> Fail#
  _  -> case derefChar8# s of
    c1 | f (C# c1) -> OK# (C# c1) (plusAddr# s 1#) n
       | otherwise -> Fail#
{-#  inline satisfyASCII #-}

-- | Parse an ASCII `Char` for which a predicate holds.
satisfyASCII_ :: (Char -> Bool) -> Parser e ()
satisfyASCII_ f = () <$ satisfyASCII f
{-# inline satisfyASCII_ #-}

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
fusedSatisfy f1 f2 f3 f4 = Parser \fp !r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# | f1 (C# c1) -> OK# (C# c1) (plusAddr# buf 1#) n
         | otherwise  -> Fail#
      _  -> case eqAddr# eob (plusAddr# buf 1#) of
        1# -> Fail#
        _ -> case indexCharOffAddr# buf 1# of
          c2 -> case c1 `leChar#` '\xDF'# of
            1# ->
              let resc = C# (chr# (((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#) `orI#`
                                   (ord# c2 -# 0x80#)))
              in case f2 resc of
                   True -> OK# resc (plusAddr# buf 2#) n
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
                         True -> OK# resc (plusAddr# buf 3#) n
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
                             True -> OK# resc (plusAddr# buf 4#) n
                             _    -> Fail#
{-# inline fusedSatisfy #-}

-- | Parse any `Word8`.
anyWord8 :: Parser e Word8
anyWord8 = Parser \fp !r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case indexWord8OffAddr# buf 0# of
    w -> OK# (W8# w) (plusAddr# buf 1#) n
{-# inline anyWord8 #-}

-- | Parse any `Word16`.
anyWord16 :: Parser e Word16
anyWord16 = Parser \fp !r eob buf n -> case 2# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexWord16OffAddr# buf 0# of
    w -> OK# (W16# w) (plusAddr# buf 2#) n
{-# inline anyWord16 #-}

-- | Parse any `Word32`.
anyWord32 :: Parser e Word32
anyWord32 = Parser \fp !r eob buf n -> case 4# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexWord32OffAddr# buf 0# of
    w -> OK# (W32# w) (plusAddr# buf 4#) n
{-# inline anyWord32 #-}

-- | Parse any `Word`.
anyWord :: Parser e Word
anyWord = Parser \fp !r eob buf n -> case 8# <=# minusAddr# eob buf of
  0# -> Fail#
  _  -> case indexWordOffAddr# buf 0# of
    w -> OK# (W# w) (plusAddr# buf 8#) n
{-# inline anyWord #-}

-- | Parse any UTF-8-encoded `Char`.
anyChar :: Parser e Char
anyChar = Parser \fp !r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# (C# c1) (plusAddr# buf 1#) n
      _  -> case eqAddr# eob (plusAddr# buf 1#) of
        1# -> Fail#
        _ -> case indexCharOffAddr# buf 1# of
          c2 -> case c1 `leChar#` '\xDF'# of
            1# ->
              let resc = ((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#) `orI#`
                          (ord# c2 -# 0x80#)
              in OK# (C# (chr# resc)) (plusAddr# buf 2#) n
            _ -> case eqAddr# eob (plusAddr# buf 2#) of
              1# -> Fail#
              _  -> case indexCharOffAddr# buf 2# of
                c3 -> case c1 `leChar#` '\xEF'# of
                  1# ->
                    let resc = ((ord# c1 -# 0xE0#) `uncheckedIShiftL#` 12#) `orI#`
                               ((ord# c2 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                (ord# c3 -# 0x80#)
                    in OK# (C# (chr# resc)) (plusAddr# buf 3#) n
                  _ -> case eqAddr# eob (plusAddr# buf 3#) of
                    1# -> Fail#
                    _  -> case indexCharOffAddr# buf 3# of
                      c4 ->
                        let resc = ((ord# c1 -# 0xF0#) `uncheckedIShiftL#` 18#) `orI#`
                                   ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 12#) `orI#`
                                   ((ord# c3 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                    (ord# c4 -# 0x80#)
                        in OK# (C# (chr# resc)) (plusAddr# buf 4#) n
{-# inline anyChar #-}

-- | Skip any UTF-8-encoded `Char`.
anyChar_ :: Parser e ()
anyChar_ = Parser \fp !r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# () (plusAddr# buf 1#) n
      _  ->
        let buf' =
              case c1 `leChar#` '\xDF'# of
                1# -> plusAddr# buf 2#
                _  -> case c1 `leChar#` '\xEF'# of
                    1# -> plusAddr# buf 3#
                    _ ->  plusAddr# buf 4#
        in case leAddr# buf' eob of
             1# -> OK# () buf' n
             _  -> Fail#
{-# inline anyChar_ #-}


-- | Parse any `Char` in the ASCII range, fail if the next input character is not in the range.
--   This is more efficient than `anyChar` if we are only working with ASCII.
anyCharASCII :: Parser e Char
anyCharASCII = Parser \fp !r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# (C# c1) (plusAddr# buf 1#) n
      _  -> Fail#
{-# inline anyCharASCII #-}

-- | Skip any `Char` in the ASCII range. More efficient than `anyChar_` if we're working only with
--   ASCII.
anyCharASCII_ :: Parser e ()
anyCharASCII_ = () <$ anyCharASCII
{-# inline anyCharASCII_ #-}

-- | Read an `Int` from the input, as a non-empty digit sequence. The `Int` may
--   overflow in the result.
readInt :: Parser e Int
readInt = Parser \fp r eob s n -> case FlatParse.Internal.readInt eob s of
  (# (##) | #)        -> Fail#
  (# | (# i, s' #) #) -> OK# (I# i) s' n
{-# inline readInt #-}

-- | Read an `Integer` from the input, as a non-empty digit sequence.
readInteger :: Parser e Integer
readInteger = Parser \fp r eob s n -> case FlatParse.Internal.readInteger fp eob s of
  (# (##) | #)        -> Fail#
  (# | (# i, s' #) #) -> OK# i s' n
{-# inline readInteger #-}


--------------------------------------------------------------------------------

-- | Choose between two parsers. If the first parser fails, try the second one, but if the first one
--   throws an error, propagate the error.
infixr 6 <|>
(<|>) :: Parser e a -> Parser e a -> Parser e a
(<|>) (Parser f) (Parser g) = Parser \fp !r eob s n ->
  case f fp r eob s n of
    Fail# -> g fp r eob s n
    x     -> x
{-# inline (<|>) #-}

-- | Branch on a parser: if the first argument succeeds, continue with the second, else with the third.
--   This can produce slightly more efficient code than `(<|>)`. Moreover, `ḃranch` does not
--   backtrack from the true/false cases.
branch :: Parser e a -> Parser e b -> Parser e b -> Parser e b
branch pa pt pf = Parser \fp !r eob s n -> case runParser# pa fp r eob s n of
  OK# _ s n -> runParser# pt fp r eob s n
  Fail#     -> runParser# pf fp r eob s n
  Err# e    -> Err# e
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
chainr f (Parser elem) (Parser end) = go where
  go = Parser \fp !r eob s n -> case elem fp r eob s n of
    OK# a s n -> case runParser# go fp r eob s n of
      OK# b s n -> let !b' = f a b in OK# b' s n
      x         -> x
    Fail# -> end fp r eob s n
    Err# e -> Err# e
{-# inline chainr #-}

-- | Run a parser zero or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
many :: Parser e a -> Parser e [a]
many (Parser f) = go where
  go = Parser \fp !r eob s n -> case f fp r eob s n of
    OK# a s n -> case runParser# go fp r eob s n of
                   OK# as s n -> OK# (a:as) s n
                   x          -> x
    Fail#  -> OK# [] s n
    Err# e -> Err# e
{-# inline many #-}

-- | Skip a parser zero or more times.
many_ :: Parser e a -> Parser e ()
many_ (Parser f) = go where
  go = Parser \fp !r eob s n -> case f fp r eob s n of
    OK# a s n -> runParser# go fp r eob s n
    Fail#     -> OK# () s n
    Err# e    -> Err# e
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

-- | Succeed if the first parser succeeds and the second one fails. The parsing
--   state is restored to the point of the first argument's success.
notFollowedBy :: Parser e a -> Parser e b -> Parser e a
notFollowedBy p1 p2 = p1 <* lookahead (fails p2)
{-# inline notFollowedBy #-}


--------------------------------------------------------------------------------

-- | Get the current position in the input.
getPos :: Parser e Pos
getPos = Parser \fp !r eob s n -> OK# (addrToPos# eob s) s n
{-# inline getPos #-}

-- | Set the input position. Warning: this can result in crashes if the position points outside the
--   current buffer. It is always safe to `setPos` values which came from `getPos` with the current
--   input.
setPos :: Pos -> Parser e ()
setPos s = Parser \fp !r eob _ n -> OK# () (posToAddr# eob s) n
{-# inline setPos #-}

-- | The end of the input.
endPos :: Pos
endPos = Pos 0
{-# inline endPos #-}


-- | Return the consumed span of a parser. Use `spanned` if possible for better efficiency.
spanOf :: Parser e a -> Parser e Span
spanOf (Parser f) = Parser \fp !r eob s n -> case f fp r eob s n of
  OK# a s' n -> OK# (Span (addrToPos# eob s) (addrToPos# eob s')) s' n
  x          -> unsafeCoerce# x
{-# inline spanOf #-}

-- | Bind the result together with the span of the result. CPS'd version of `spanOf`
--   for better unboxing.
spanned :: Parser e a -> (a -> Span -> Parser e b) -> Parser e b
spanned (Parser f) g = Parser \fp !r eob s n -> case f fp r eob s n of
  OK# a s' n -> runParser# (g a (Span (addrToPos# eob s) (addrToPos# eob s'))) fp r eob s' n
  x          -> unsafeCoerce# x
{-# inline spanned #-}

-- | Return the `B.ByteString` consumed by a parser. Note: it's more efficient to use `spanOf` and
--   `spanned` instead.
byteStringOf :: Parser e a -> Parser e B.ByteString
byteStringOf (Parser f) = Parser \fp !r eob s n -> case f fp r eob s n of
  OK# a s' n -> OK# (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) s' n
  x          -> unsafeCoerce# x
{-# inline byteStringOf #-}

-- | CPS'd version of `byteStringOf`. Can be more efficient, because the result is more eagerly unboxed
--   by GHC. It's more efficient to use `spanOf` or `spanned` instead.
byteStringed :: Parser e a -> (a -> B.ByteString -> Parser e b) -> Parser e b
byteStringed (Parser f) g = Parser \fp !r eob s n -> case f fp r eob s n of
  OK# a s' n -> runParser# (g a (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s)))) fp r eob s' n
  x          -> unsafeCoerce# x
{-# inline byteStringed #-}

-- | Run a parser in a given input span. The input position and the `Int` state is restored after
--   the parser is finished, so `inSpan` does not consume input and has no side effect.  Warning:
--   this operation may crash if the given span points outside the current parsing buffer. It's
--   always safe to use `inSpan` if the span comes from a previous `spanned` or `spanOf` call on
--   the current input.
inSpan :: Span -> Parser e a -> Parser e a
inSpan (Span s eob) (Parser f) = Parser \fp !r eob' s' n' ->
  case f fp r (posToAddr# eob' eob) (posToAddr# eob' s) n' of
    OK# a _ _ -> OK# a s' n'
    x         -> unsafeCoerce# x
{-# inline inSpan #-}

--------------------------------------------------------------------------------

-- | Create a `B.ByteString` from a `Span`. The result is invalid is the `Span` points
--   outside the current buffer, or if the `Span` start is greater than the end position.
unsafeSpanToByteString :: Span -> Parser e B.ByteString
unsafeSpanToByteString (Span l r) =
  lookahead (setPos l >> byteStringOf (setPos r))
{-# inline unsafeSpanToByteString #-}

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
unpackUTF8 str = case runParser takeRest 0 0 str of
  OK a _ _ -> a
  _        -> error "unpackUTF8: invalid encoding"

-- | Check that the input has at least the given number of bytes.
ensureBytes# :: Int -> Parser e ()
ensureBytes# (I# len) = Parser \fp !r eob s n ->
  case len  <=# minusAddr# eob s of
    1# -> OK# () s n
    _  -> Fail#
{-# inline ensureBytes# #-}

-- | Unsafely read a concrete byte from the input. It's not checked that the input has
--   enough bytes.
scan8# :: Word -> Parser e ()
scan8# (W# c) = Parser \fp !r eob s n ->
  case indexWord8OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 1#) n
      _  -> Fail#
{-# inline scan8# #-}

-- | Unsafely read two concrete bytes from the input. It's not checked that the input has
--   enough bytes.
scan16# :: Word -> Parser e ()
scan16# (W# c) = Parser \fp !r eob s n ->
  case indexWord16OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 2#) n
      _  -> Fail#
{-# inline scan16# #-}

-- | Unsafely read four concrete bytes from the input. It's not checked that the input has
--   enough bytes.
scan32# :: Word -> Parser e ()
scan32# (W# c) = Parser \fp !r eob s n ->
  case indexWord32OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 4#) n
      _  -> Fail#
{-# inline scan32# #-}

-- | Unsafely read eight concrete bytes from the input. It's not checked that the input has
--   enough bytes.
scan64# :: Word -> Parser e ()
scan64# (W# c) = Parser \fp !r eob s n ->
  case indexWord64OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 8#) n
      _  -> Fail#
{-# inline scan64# #-}

-- | Unsafely read and return a byte from the input. It's not checked that the input is non-empty.
scanAny8# :: Parser e Word8
scanAny8# = Parser \fp !r eob s n -> OK# (W8# (indexWord8OffAddr# s 0#)) (plusAddr# s 1#) n
{-# inline scanAny8# #-}

scanPartial64# :: Int -> Word -> Parser e ()
scanPartial64# (I# len) (W# w) = Parser \fp !r eob s n ->
  case indexWordOffAddr# s 0# of
    w' -> case uncheckedIShiftL# (8# -# len) 3# of
      sh -> case uncheckedShiftL# w' sh of
        w' -> case uncheckedShiftRL# w' sh of
          w' -> case eqWord# w w' of
            1# -> OK# () (plusAddr# s len) n
            _  -> Fail#
{-# inline scanPartial64# #-}

-- | Decrease the current input position by the given number of bytes.
setBack# :: Int -> Parser e ()
setBack# (I# i) = Parser \fp !r eob s n ->
  OK# () (plusAddr# s (negateInt# i)) n
{-# inline setBack# #-}

-- | Template function, creates a @Parser e ()@ which unsafely scans a given
--   sequence of bytes.
scanBytes# :: [Word8] -> Q Exp
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

              let cases = DoE Nothing $
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

      !m    =  M.fromList ((Nothing, maybe (VarE 'empty) id fallback) : branches)
      !trie = compileTrie strings
  in (m , trie)
