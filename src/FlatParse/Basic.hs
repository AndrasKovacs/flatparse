{-# language UnboxedTuples #-}

{-|
This module implements a `Parser` supporting custom error types.  If you need efficient indentation
parsing, use "FlatParse.Stateful" instead.

Many internals are exposed for hacking on and extending. These are generally
denoted by a @#@ hash suffix.
-}

module FlatParse.Basic (

  -- * Parser monad
    type Parser

  -- ** Executing parsers
  , Result(..)
  , runParser
  , runParserS

  -- * Errors and failures
  , err
  , lookahead
  , fails
  , try
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

  -- * Primitive parsers
  , eof
  , switch
  , switchWithPost
  , rawSwitchWithPost

  -- ** Byte-wise
  , take
  , takeRest
  , skip
  , getBytesOf
  , getByteStringOf
  , getCString

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

  -- ** Position and span conversions
  , validPos
  , posLineCols
  , unsafeSpanToByteString
  , mkPos
  , FlatParse.Basic.lines

  -- * Getting the rest of the input as a 'String'
  , takeLine
  , traceLine
  , takeRestString
  , traceRestString

  -- * `String` conversions
  , packUTF8
  , unpackUTF8

  -- * Internal functions
  , ensureBytes#

  -- ** Unboxed arguments
  , take#
  , atSkip#


  -- ** Location & address primitives
  , setBack#
  , module FlatParse.Basic.Addr

  -- ** Unsafe
  , getCStringUnsafe

  ) where

import Prelude hiding ( take, getChar )

import Control.Applicative

import Control.Monad
import Data.Foldable
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import GHC.Exts
import GHC.Word
import GHC.ForeignPtr ( ForeignPtr(..) )
import Language.Haskell.TH
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import qualified Data.Map.Strict as M

import qualified FlatParse.Common.Numbers as Common
import qualified FlatParse.Common.Assorted as Common
import FlatParse.Common.Position
import FlatParse.Common.Trie
import FlatParse.Common.Assorted ( packBytes, splitBytes, strToBytes, packUTF8 )

import FlatParse.Basic.Parser
import FlatParse.Basic.Integers
import FlatParse.Basic.Internal
import FlatParse.Basic.Chars
import FlatParse.Basic.Position
import FlatParse.Basic.Addr

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


--------------------------------------------------------------------------------

-- | Throw a parsing error. By default, parser choice `(<|>)` can't backtrack
--   on parser error. Use `try` to convert an error to a recoverable failure.
err :: e -> Parser e a
err e = Parser \fp eob s -> Err# e
{-# inline err #-}

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

-- | Convert a parsing failure to a `()`.
optional_ :: Parser e a -> Parser e ()
optional_ p = (() <$ p) <|> pure ()
{-# inline optional_ #-}

-- | CPS'd version of `optional`. This is usually more efficient, since it gets
--   rid of the extra `Maybe` allocation.
withOption :: Parser e a -> (a -> Parser e r) -> Parser e r -> Parser e r
withOption (Parser f) just (Parser nothing) = Parser \fp eob s -> case f fp eob s of
  OK# a s -> runParser# (just a) fp eob s
  Fail#   -> nothing fp eob s
  Err# e  -> Err# e
{-# inline withOption #-}

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
take :: Int -> Parser e B.ByteString
take (I# n#) = take# n#
{-# inline take #-}

-- | Consume the rest of the input. May return the empty bytestring.
takeRest :: Parser e B.ByteString
takeRest = Parser \fp eob s ->
  let n# = minusAddr# eob s
  in  OK# (B.PS (ForeignPtr s fp) 0 (I# n#)) eob
{-# inline takeRest #-}

-- | Skip forward @n@ bytes. Fails if fewer than @n@ bytes are available.
--
-- Throws a runtime error if given a negative integer.
skip :: Int -> Parser e ()
skip (I# os#) = atSkip# os# (pure ())
{-# inline skip #-}

-- | Parse a UTF-8 character literal. This is a template function, you can use it as
--   @$(char \'x\')@, for example, and the splice in this case has type @Parser e ()@.
getCharOf :: Char -> Q Exp
getCharOf c = getStringOf [c]

-- | Parse a UTF-8 string literal. This is a template function, you can use it as @$(string "foo")@,
--   for example, and the splice has type @Parser e ()@.
getStringOf :: String -> Q Exp
getStringOf str = getBytesOf (strToBytes str)

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

-- | Read a non-negative `Int` from the input, as a non-empty digit sequence.
-- The `Int` may overflow in the result.
getAsciiDecimalInt :: Parser e Int
getAsciiDecimalInt = Parser \fp eob s -> case Common.readInt eob s of
  (# (##) | #)        -> Fail#
  (# | (# n, s' #) #) -> OK# (I# n) s'
{-# inline getAsciiDecimalInt #-}

-- | Read an `Int` from the input, as a non-empty case-insensitive ASCII
--   hexadecimal digit sequence. The `Int` may overflow in the result.
getAsciiHexInt :: Parser e Int
getAsciiHexInt = Parser \fp eob s -> case Common.readIntHex eob s of
  (# (##) | #)        -> Fail#
  (# | (# n, s' #) #) -> OK# (I# n) s'
{-# inline getAsciiHexInt #-}

-- | Read a non-negative `Integer` from the input, as a non-empty digit
-- sequence.
getAsciiDecimalInteger :: Parser e Integer
getAsciiDecimalInteger = Parser \fp eob s -> case Common.readInteger fp eob s of
  (# (##) | #)        -> Fail#
  (# | (# i, s' #) #) -> OK# i s'
{-# inline getAsciiDecimalInteger #-}

--------------------------------------------------------------------------------

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

-- | Skip a parser zero or more times.
many_ :: Parser e a -> Parser e ()
many_ (Parser f) = Parser go where
  go fp eob s = case f fp eob s of
    OK# a s -> go fp eob s
    Fail#   -> OK# () s
    Err# e  -> Err# e
{-# inline many_ #-}

-- | Skip a parser one or more times.
some_ :: Parser e a -> Parser e ()
some_ pa = pa >> many_ pa
{-# inline some_ #-}

-- | Succeed if the first parser succeeds and the second one fails.
notFollowedBy :: Parser e a -> Parser e b -> Parser e a
notFollowedBy p1 p2 = p1 <* fails p2
{-# inline notFollowedBy #-}

--------------------------------------------------------------------------------

-- | Parse a given `B.ByteString`. If the bytestring is statically known,
--   consider using 'bytes' instead.
getByteStringOf :: B.ByteString -> Parser e ()
getByteStringOf (B.PS (ForeignPtr bs fcontent) _ (I# len)) =

  let go64 :: Addr# -> Addr# -> Addr# -> State# RealWorld -> (# Res# e (), State# RealWorld #)
      go64 bs bsend s w =
        let bs' = plusAddr# bs 8# in
        case gtAddr# bs' bsend of
          1# -> go8 bs bsend s w
          _  -> if   W64# (indexWord64OffAddr# bs 0#) == W64# (indexWord64OffAddr# s 0#)
                then go64 bs' bsend (plusAddr# s 8#) w
                else (# Fail#, w #)

      go8 :: Addr# -> Addr# -> Addr# -> State# RealWorld -> (# Res# e (), State# RealWorld #)
      go8 bs bsend s w =
        case ltAddr# bs bsend of
          1# -> if   W8# (indexWord8OffAddr# bs 0#) == W8# (indexWord8OffAddr# s 0#)
                then go8 (plusAddr# bs 1#) bsend (plusAddr# s 1#) w
                else (# Fail#, w #)
          _  -> (# OK# () s, w #)

  in Parser \fp eob s -> case len <=# minusAddr# eob s of
       1# -> runRW# \w -> case go64 bs (plusAddr# bs len) s w of
               (# res, w #) -> case touch# fcontent w of
                 w -> res
       _  -> Fail#
{-# inline getByteStringOf #-}

-- | Read a sequence of bytes. This is a template function, you can use it as
--   @$(getBytesOf [3, 4, 5])@, for example, and the splice has type @Parser e
--   ()@.
getBytesOf :: [Word] -> Q Exp
getBytesOf bytes = do
  let !len = length bytes
  [| ensureBytes# len >> $(scanBytes# bytes) |]

-- | Template function, creates a @Parser e ()@ which unsafely scans a given
--   sequence of bytes.
scanBytes# :: [Word] -> Q Exp
scanBytes# bytes = do
  let !(leading, w8s) = splitBytes bytes
      !scanw8s        = go w8s where
                         go (w8:[] ) = [| getWord64OfUnsafe w8 |]
                         go (w8:w8s) = [| getWord64OfUnsafe w8 >> $(go w8s) |]
                         go []       = [| pure () |]
  case w8s of
    [] -> go leading
          where
            go (a:b:c:d:[]) = let !w = packBytes [a, b, c, d] in [| getWord32OfUnsafe w |]
            go (a:b:c:d:ws) = let !w = packBytes [a, b, c, d] in [| getWord32OfUnsafe w >> $(go ws) |]
            go (a:b:[])     = let !w = packBytes [a, b]       in [| getWord16OfUnsafe w |]
            go (a:b:ws)     = let !w = packBytes [a, b]       in [| getWord16OfUnsafe w >> $(go ws) |]
            go (a:[])       = [| getWord8OfUnsafe a |]
            go []           = [| pure () |]
    _  -> case leading of

      []              -> scanw8s
      [a]             -> [| getWord8OfUnsafe a >> $scanw8s |]
      ws@[a, b]       -> let !w = packBytes ws in [| getWord16OfUnsafe w >> $scanw8s |]
      ws@[a, b, c, d] -> let !w = packBytes ws in [| getWord32OfUnsafe w >> $scanw8s |]
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
                    [BindS (VarP (mkName "c")) (VarE 'getWord8Unsafe),
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

-- | Branch on a parser: if the first argument succeeds, continue with the second, else with the third.
--   This can produce slightly more efficient code than `(<|>)`. Moreover, `ḃranch` does not
--   backtrack from the true/false cases.
branch :: Parser e a -> Parser e b -> Parser e b -> Parser e b
branch pa pt pf = Parser \fp eob s -> case runParser# pa fp eob s of
  OK# _ s -> runParser# pt fp eob s
  Fail#   -> runParser# pf fp eob s
  Err# e  -> Err# e
{-# inline branch #-}

-- | @isolate n p@ runs the parser @p@ isolated to the next @n@ bytes. All
--   isolated bytes must be consumed.
--
-- Throws a runtime error if given a negative integer.
isolate :: Int -> Parser e a -> Parser e a
isolate (I# n#) p = withPosInt# n# (\n'# -> isolateUnsafe# n'# p)
{-# inline isolate #-}

-- | @isolate n p@ runs the parser @p@ isolated to the next @n@ bytes. All
--   isolated bytes must be consumed.
--
-- Undefined behaviour if given a negative integer.
isolateUnsafe# :: Int# -> Parser e a -> Parser e a
isolateUnsafe# n# p = Parser \fp eob s ->
  let s' = plusAddr# s n#
  in  case n# <=# minusAddr# eob s of
        1# -> case runParser# p fp s' s of
          OK# a s'' -> case eqAddr# s' s'' of
            1# -> OK# a s''
            _  -> Fail# -- isolated segment wasn't fully consumed
          Fail#     -> Fail#
          Err# e    -> Err# e
        _  -> Fail# -- you tried to isolate more than we have left
{-# inline isolateUnsafe# #-}

--------------------------------------------------------------------------------

-- | Convert an UTF-8-coded `B.ByteString` to a `String`.
unpackUTF8 :: B.ByteString -> String
unpackUTF8 str = case runParser takeRestString str of
  OK a _ -> a
  _      -> error "unpackUTF8: invalid encoding"

-- | Take the rest of the input as a `String`. Assumes UTF-8 encoding.
takeRestString :: Parser e String
takeRestString = branch eof (pure "") do
  c <- getChar
  cs <- takeRestString
  pure (c:cs)

-- | Get the rest of the input as a `String`, but restore the parsing state. Assumes UTF-8 encoding.
--   This can be used for debugging.
traceRestString :: Parser e String
traceRestString = lookahead takeRestString

--------------------------------------------------------------------------------

-- | Parse the rest of the current line as a `String`. Assumes UTF-8 encoding,
--   throws an error if the encoding is invalid.
takeLine :: Parser e String
takeLine = branch eof (pure "") do
  c <- getChar
  case c of
    '\n' -> pure ""
    _    -> (c:) <$> takeLine

-- | Parse the rest of the current line as a `String`, but restore the parsing state.
--   Assumes UTF-8 encoding. This can be used for debugging.
traceLine :: Parser e String
traceLine = lookahead takeLine

-- | Run a parser on a `String` input. Reminder: @OverloadedStrings@ for `B.ByteString` does not
--   yield a valid UTF-8 encoding! For non-ASCII `B.ByteString` literal input, use `runParserS` or
--   `packUTF8` for testing.
runParserS :: Parser e a -> String -> Result e a
runParserS pa s = runParser pa (packUTF8 s)

-- | Create a `Pos` from a line and column number. Throws an error on out-of-bounds
--   line and column numbers.
mkPos :: B.ByteString -> (Int, Int) -> Pos
mkPos str (line', col') =
  let go line col | line == line' && col == col' = getPos
      go line col = (do
        c <- getChar
        if c == '\n' then go (line + 1) 0
                     else go line (col + 1)) <|> error "mkPos: invalid position"
  in case runParser (go 0 0) str of
    OK res _ -> res
    _        -> error "impossible"

-- | Break an UTF-8-coded `B.ByteString` to lines. Throws an error on invalid
--   input. This is mostly useful for grabbing specific source lines for
--   displaying error messages.
lines :: B.ByteString -> [String]
lines str =
  let go = ([] <$ eof) <|> ((:) <$> takeLine <*> go)
  in case runParser go str of
    OK ls _ -> ls
    _       -> error "linesUTF8: invalid input"


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
          c <- getChar
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

--------------------------------------------------------------------------------
-- Low-level boxed combinators

-- | Read a null-terminated bytestring (a C-style string).
--
-- Consumes the null terminator.
getCString :: Parser e B.ByteString
getCString = Parser \fp eob s -> go' fp eob s
  where
    go' fp eob s0 = go 0# s0
      where
        go n# s = case eqAddr# eob s of
          1# -> Fail#
          _  ->
            let s' = plusAddr# s 1#
                w# = indexWord8OffAddr# s 0#
            in  if   W8# w# == 0x00
                then OK# (B.PS (ForeignPtr s0 fp) 0 (I# n#)) s'
                else go (n# +# 1#) s'
{-# inline getCString #-}

-- | Read a null-terminated bytestring (a C-style string), where the bytestring
--   is known to be null-terminated somewhere in the input.
--
-- Undefined behaviour if your bytestring isn't null-terminated somewhere.
-- You almost certainly want 'getCString' instead.
--
-- Fails on GHC versions older than 9.0, since we make use of the
-- 'cstringLength#' primop introduced in GHC 9.0, and we aren't very useful
-- without it.
--
-- Consumes the null terminator.
getCStringUnsafe :: Parser e B.ByteString
{-# inline getCStringUnsafe #-}
#if MIN_VERSION_base(4,15,0)
getCStringUnsafe = Parser \fp eob s ->
  case eqAddr# eob s of
    1# -> Fail#
    _  -> let n#  = cstringLength# s
              s'# = plusAddr# s (n# +# 1#)
           in OK# (B.PS (ForeignPtr s fp) 0 (I# n#)) s'#
#else
getCStringUnsafe = error "Flatparse.Basic.getCStringUnsafe: requires GHC 9.0 / base-4.15, not available on this compiler"
#endif
