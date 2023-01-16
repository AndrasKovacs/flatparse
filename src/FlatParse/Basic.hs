{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
This module implements a `Parser` supporting custom error types.  If you need efficient indentation
parsing, use "FlatParse.Stateful" instead.

Many internals are exposed for hacking on and extending. These are generally
denoted by a @#@ hash suffix.
-}

module FlatParse.Basic (

  -- * TODO
    module FlatParse.Basic
  , module FlatParse.Basic.Parser
  , module FlatParse.Basic.Base
  , module FlatParse.Basic.Integers
  , module FlatParse.Basic.Strings
  , module FlatParse.Basic.Addr
  , module FlatParse.Common.Position

  , Control.Applicative.empty

  ) where

import Prelude hiding ( take )

import qualified Control.Applicative
import Control.Monad
import Data.Foldable
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import GHC.IO (IO(..))
import GHC.Exts
import GHC.ForeignPtr
import Language.Haskell.TH
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import qualified Data.Map.Strict as M

import FlatParse.Internal
import FlatParse.Internal.UnboxedNumerics

import FlatParse.Basic.Parser
import FlatParse.Basic.Base
import FlatParse.Basic.Integers
import FlatParse.Basic.Strings
import FlatParse.Basic.Addr
import FlatParse.Common.Position

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

-- | Embed an IO action in a 'ParserT'. This is slightly safer than 'unsafePerformIO' because
-- it will sequenced correctly with respect to the surrounding actions, and its execution is guaranteed.
unsafeLiftIO :: IO a -> ParserT st e a
unsafeLiftIO io = ParserT \fp eob s st ->
                   let !a = unsafePerformIO io
                   in OK# st a s
{-# inline unsafeLiftIO #-}

--------------------------------------------------------------------------------

-- | Run a parser.
runParser :: Parser e a -> B.ByteString -> Result e a
runParser (ParserT f) b@(B.PS (ForeignPtr _ fp) _ (I# len)) = unsafePerformIO $
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let end = plusAddr# buf len
    pure case f fp end buf proxy# of
      OK# _st a s -> let offset = minusAddr# s buf
                     in OK a (B.drop (I# offset) b)

      Err# _st e -> Err e
      Fail# _st  -> Fail
{-# noinline runParser #-}
-- We mark this as noinline to allow power users to safely do unsafe state token coercions.
-- Details are discussed in https://github.com/AndrasKovacs/flatparse/pull/34#issuecomment-1326999390

-- | Run an ST based parser
runParserST :: (forall s. ParserST s e a) -> B.ByteString -> Result e a
runParserST pst buf = unsafeDupablePerformIO (runParserIO pst buf)
{-# inlinable runParserST #-}

-- | Run an IO based parser
runParserIO :: ParserIO e a -> B.ByteString -> IO (Result e a)
runParserIO (ParserT f) b@(B.PS (ForeignPtr _ fp) _ (I# len)) = do
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let end = plusAddr# buf len
    IO \st -> case f fp end buf st of
      OK# rw' a s ->  let offset = minusAddr# s buf
                      in (# rw', OK a (B.drop (I# offset) b) #)

      Err# rw' e ->  (# rw', Err e #)
      Fail# rw'  ->  (# rw', Fail #)
{-# inlinable runParserIO #-}


-- | Run a parser on a `String` input. Reminder: @OverloadedStrings@ for `B.ByteString` does not
--   yield a valid UTF-8 encoding! For non-ASCII `B.ByteString` literal input, use `runParserS` or
--   `packUTF8` for testing.
runParserS :: Parser e a -> String -> Result e a
runParserS pa s = runParser pa (packUTF8 s)

--------------------------------------------------------------------------------

-- | Convert a parsing failure to a `Maybe`. If possible, use `withOption` instead.
optional :: ParserT st e a -> ParserT st e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# inline optional #-}

--------------------------------------------------------------------------------

-- | Parse a given `B.ByteString`. If the bytestring is statically known, consider using 'bytes' instead.
byteString :: B.ByteString -> ParserT st e ()
byteString (B.PS (ForeignPtr bs fcontent) _ (I# len)) =

  let go64 :: Addr# -> Addr# -> Addr# -> State# RealWorld -> Res# (State# RealWorld) e ()
      go64 bs bsend s rw =
        let bs' = plusAddr# bs 8# in
        case gtAddr# bs' bsend of
          1# -> go8 bs bsend s rw
#if MIN_VERSION_base(4,17,0)
          _  -> case eqWord64# (indexWord64OffAddr# bs 0#) (indexWord64OffAddr# s 0#) of
#else
          _  -> case eqWord# (indexWord64OffAddr# bs 0#) (indexWord64OffAddr# s 0#) of
#endif
            1# -> go64 bs' bsend (plusAddr# s 8#) rw
            _  -> Fail# rw

      go8 :: Addr# -> Addr# -> Addr# -> State# RealWorld -> Res# (State# RealWorld) e ()
      go8 bs bsend s rw = case ltAddr# bs bsend of
        1# -> case eqWord8'# (indexWord8OffAddr# bs 0#) (indexWord8OffAddr# s 0#) of
          1# -> go8 (plusAddr# bs 1#) bsend (plusAddr# s 1#) rw
          _  -> Fail# rw
        _  -> OK# rw () s

      go :: Addr# -> Addr# -> Addr# -> State# RealWorld -> Res# (State# RealWorld) e ()
      go bs bsend s rw = case go64 bs bsend s rw of
        (# rw', res #) -> case touch# fcontent rw' of
          rw'' -> (# rw'', res #)

  in ParserT \fp eob s st ->
      case len <=# minusAddr# eob s of
           1# -> case runRW# (go bs (plusAddr# bs len) s) of
             (# rw, a #) -> (# st, a #)
           _  -> Fail# st
{-# inline byteString #-}

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
@ws :: ParserT st e ()@ for a whitespace parser, we might want to consume whitespace after matching
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
-- Fails on overflow.
readInt :: ParserT st e Int
readInt = ParserT \fp eob s st -> case FlatParse.Internal.readInt eob s of
  (# (##) | #)        -> Fail# st
  (# | (# n, s' #) #) -> OK# st (I# n) s'
{-# inline readInt #-}

-- | Read an `Int` from the input, as a non-empty case-insensitive ASCII
--   hexadecimal digit sequence.
-- Fails on overflow.
readIntHex :: ParserT st e Int
readIntHex = ParserT $ \fp eob s st ->
  case FlatParse.Internal.readIntHex eob s of
    (# | (# i, s' #) #) -> OK# st (I# i) s'
    (# (# #) | #)       -> Fail# st
{-# inline readIntHex #-}

-- | Read a `Word` from the input, as a non-empty digit sequence.
-- Fails on overflow.
readWord :: ParserT st e Int
readWord = ParserT \fp eob s st -> case FlatParse.Internal.readInt eob s of
  (# (##) | #)        -> Fail# st
  (# | (# n, s' #) #) -> OK# st (I# n) s'
{-# inline readWord #-}

readWordHex :: ParserT st e Word
readWordHex = ParserT $ \_fp eob s st ->
  case FlatParse.Internal.readWordHex eob s of
    (# | (# n, s' #) #) -> OK# st (W# n) s'
    (# (# #) | #)       -> Fail# st
{-# inline readWordHex #-}

-- | Read a non-negative `Integer` from the input, as a non-empty digit
-- sequence.
readInteger :: ParserT st e Integer
readInteger = ParserT \fp eob s st -> case FlatParse.Internal.readInteger fp eob s of
  (# (##) | #)        -> Fail# st
  (# | (# i, s' #) #) -> OK# st i s'
{-# inline readInteger #-}

-- | Read a protobuf-style varint into an `Word`.
--
-- protobuf-style varints are byte-aligned. For each byte, the lower 7 bits are
-- data and the MSB indicates if there are further bytes. Once fully parsed, the
-- 7-bit payloads are concatenated and interpreted as a little-endian unsigned
-- integer.
--
-- Really, these are varnats. They also match with the LEB128 varint encoding.
--
-- protobuf encodes negatives in unsigned integers using zigzag encoding. See
-- the @fromZigzag@ family of functions for this functionality.
--
-- Further reading:
-- https://developers.google.com/protocol-buffers/docs/encoding#varints
readVarintProtobuf :: ParserT st e Word
readVarintProtobuf = ParserT \fp eob s st ->
    case readVarintProtobuf# eob s of
      (# (##) | #) -> Fail# st
      (# | (# w#, s#, n# #) #) ->
        case n# ># 64# of
          1# -> Fail# st -- overflow
          _  -> OK# st (W# w#) s#
{-# inline readVarintProtobuf #-}

--------------------------------------------------------------------------------

-- | Run a parser zero or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
many :: ParserT st e a -> ParserT st e [a]
many (ParserT f) = ParserT go where
  go fp eob s st = case f fp eob s st of
    OK# st' a s -> case go fp eob s st' of
                    OK# st'' as s -> OK# st'' (a:as) s
                    x        -> x
    Fail# st'  -> OK# st' [] s
    Err# st' e -> Err# st' e
{-# inline many #-}

-- | Run a parser one or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
some :: ParserT st e a -> ParserT st e [a]
some p = (:) <$> p <*> many p
{-# inline some #-}

--------------------------------------------------------------------------------

-- | Get the current position in the input.
getPos :: ParserT st e Pos
getPos = ParserT \fp eob s st -> OK# st (addrToPos# eob s) s
{-# inline getPos #-}

-- | Set the input position. Warning: this can result in crashes if the position points outside the
--   current buffer. It is always safe to `setPos` values which came from `getPos` with the current
--   input.
setPos :: Pos -> ParserT st e ()
setPos s = ParserT \fp eob _ st -> OK# st () (posToAddr# eob s)
{-# inline setPos #-}

-- | The end of the input.
endPos :: Pos
endPos = Pos 0
{-# inline endPos #-}

-- | Return the consumed span of a parser.
spanOf :: ParserT st e a -> ParserT st e Span
spanOf (ParserT f) = ParserT \fp eob s st -> case f fp eob s st of
  OK# st' a s' -> OK# st' (Span (addrToPos# eob s) (addrToPos# eob s')) s'
  x        -> unsafeCoerce# x
{-# inline spanOf #-}

-- | Bind the result together with the span of the result. CPS'd version of `spanOf`
--   for better unboxing.
withSpan :: ParserT st e a -> (a -> Span -> ParserT st e b) -> ParserT st e b
withSpan (ParserT f) g = ParserT \fp eob s st -> case f fp eob s st of
  OK# st' a s' -> runParserT# (g a (Span (addrToPos# eob s) (addrToPos# eob s'))) fp eob s' st'
  x        -> unsafeCoerce# x
{-# inline withSpan #-}

-- | Return the `B.ByteString` consumed by a parser. Note: it's more efficient to use `spanOf` and
--   `withSpan` instead.
byteStringOf :: ParserT st e a -> ParserT st e B.ByteString
byteStringOf (ParserT f) = ParserT \fp eob s st -> case f fp eob s st of
  OK# st' a s' -> OK# st' (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) s'
  x        -> unsafeCoerce# x
{-# inline byteStringOf #-}

-- | CPS'd version of `byteStringOf`. Can be more efficient, because the result is more eagerly unboxed
--   by GHC. It's more efficient to use `spanOf` or `withSpan` instead.
withByteString :: ParserT st e a -> (a -> B.ByteString -> ParserT st e b) -> ParserT st e b
withByteString (ParserT f) g = ParserT \fp eob s st -> case f fp eob s st of
  OK# st' a s' -> runParserT# (g a (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s)))) fp eob s' st'
  x        -> unsafeCoerce# x
{-# inline withByteString #-}

-- | Run a parser in a given input span. The input position and the `Int` state is restored after
--   the parser is finished, so `inSpan` does not consume input and has no side effect.  Warning:
--   this operation may crash if the given span points outside the current parsing buffer. It's
--   always safe to use `inSpan` if the span comes from a previous `withSpan` or `spanOf` call on
--   the current input.
inSpan :: Span -> ParserT st e a -> ParserT st e a
inSpan (Span s eob) (ParserT f) = ParserT \fp eob' s' st ->
  case f fp (posToAddr# eob' eob) (posToAddr# eob' s) st of
    OK# st' a _ -> OK# st' a s'
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
unsafeSpanToByteString :: Span -> ParserT st e B.ByteString
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
takeLine :: ParserT st e String
takeLine = branch eof (pure "") do
  c <- anyChar
  case c of
    '\n' -> pure ""
    _    -> (c:) <$> takeLine

-- | Parse the rest of the current line as a `String`, but restore the parsing state.
--   Assumes UTF-8 encoding. This can be used for debugging.
traceLine :: ParserT st e String
traceLine = lookahead takeLine

-- | Take the rest of the input as a `String`. Assumes UTF-8 encoding.
takeRestString :: ParserT st e String
takeRestString = branch eof (pure "") do
  c <- anyChar
  cs <- takeRestString
  pure (c:cs)

-- | Get the rest of the input as a `String`, but restore the parsing state. Assumes UTF-8 encoding.
--   This can be used for debugging.
traceRest :: ParserT st e String
traceRest = lookahead takeRestString

--------------------------------------------------------------------------------

-- | Convert an UTF-8-coded `B.ByteString` to a `String`.
unpackUTF8 :: B.ByteString -> String
unpackUTF8 str = case runParser takeRestString str of
  OK a _ -> a
  _      -> error "unpackUTF8: invalid encoding"

-- | Decrease the current input position by the given number of bytes.
setBack# :: Int -> ParserT st e ()
setBack# (I# i) = ParserT \fp eob s st ->
  OK# st () (plusAddr# s (negateInt# i))
{-# inline setBack# #-}

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

  let ensure' :: Maybe Int -> Maybe (Q Exp)
      ensure' = fmap (\n -> [| ensure n |])

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
                    [BindS (VarP (mkName "c")) (VarE 'anyWord8Unsafe),
                      NoBindS (CaseE (VarE (mkName "c"))
                         (map (\(w, t) ->
                                 Match (LitP (IntegerL (fromIntegral w)))
                                       (NormalB t)
                                       [])
                              next
                          ++ [Match WildP (NormalB defaultCase) []]))]

              case ensure' alloc of
                Nothing    -> pure cases
                Just alloc -> [| branch $alloc $(pure cases) $(fallback r n) |]

        Path (r, n, alloc) ws t ->
          case ensure' alloc of
            Nothing    -> [| branch $(bytes ws) $(go t) $(fallback r n)|]
            Just alloc -> [| branch ($alloc >> $(bytesUnsafe ws)) $(go t) $(fallback r n) |]

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

      !m    = M.fromList ((Nothing, maybe (VarE 'failed) id fallback) : branches)
      !trie = compileTrie strings
  in (m , trie)

--------------------------------------------------------------------------------

-- | Read a null-terminated bytestring (a C-style string).
--
-- Consumes the null terminator.
anyCString :: ParserT st e B.ByteString
anyCString = ParserT go'
  where
    go' fp eob s0 st = go 0# s0
      where
        go n# s = case eqAddr# eob s of
          1# -> Fail# st
          _  ->
            let s' = plusAddr# s 1#
            -- TODO below is a candidate for improving with ExtendedLiterals!
            in  case eqWord8# (indexWord8OffAddr''# s 0#) (wordToWord8''# 0##) of
                  1# -> OK# st (B.PS (ForeignPtr s0 fp) 0 (I# n#)) s'
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
anyCStringUnsafe :: ParserT st e B.ByteString
{-# inline anyCStringUnsafe #-}
#if MIN_VERSION_base(4,15,0)
anyCStringUnsafe = ParserT \fp eob s st ->
  case eqAddr# eob s of
    1# -> Fail# st
    _  -> let n#  = cstringLength# s
              s'# = plusAddr# s (n# +# 1#)
           in OK# st (B.PS (ForeignPtr s fp) 0 (I# n#)) s'#
#else
anyCStringUnsafe = error "Flatparse.Basic.anyCStringUnsafe: requires GHC 9.0 / base-4.15, not available on this compiler"
#endif
