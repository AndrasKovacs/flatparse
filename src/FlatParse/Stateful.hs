{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
This module implements a `Parser` supporting a custom reader environment, custom
error types and an `Int` state.
-}

module FlatParse.Stateful (

  -- * TODO
    module FlatParse.Stateful
  , module FlatParse.Stateful.Parser
  , module FlatParse.Stateful.Base
  , module FlatParse.Stateful.Integers
  , module FlatParse.Stateful.Strings
  , module FlatParse.Stateful.Addr
  , module FlatParse.Common.Position

  , Control.Applicative.empty

  -- * TODO possibly remove
  , packUTF8

  -- ** Position and span conversions
  , Basic.validPos
  , Basic.posLineCols
  , Basic.mkPos
  , Basic.linesUTF8

  ) where

import qualified Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Map (Map)
import GHC.Exts
import GHC.Word
import GHC.IO (IO(..))
import Language.Haskell.TH
import System.IO.Unsafe
import GHC.ForeignPtr

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Map.Strict as M

import FlatParse.Internal

import qualified FlatParse.Parsers as Basic

import FlatParse.Stateful.Parser
import FlatParse.Stateful.Base
import FlatParse.Stateful.Integers
import FlatParse.Stateful.Strings
import FlatParse.Stateful.Addr
import FlatParse.Common.Position
import FlatParse.Common.Assorted

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

-- | Embed an IO action in a 'ParserT'. This is slightly safer than 'unsafePerformIO' because
-- it will sequenced correctly with respect to the surrounding actions, and its execution is guaranteed.
unsafeLiftIO :: IO a -> ParserT st r e a
unsafeLiftIO io = ParserT \fp !r eob s n st ->
                   let !a = unsafePerformIO io
                   in OK# st a s n
{-# inline unsafeLiftIO #-}

--------------------------------------------------------------------------------

-- | Run a parser. The `Int` argument is the initial state.
runParser :: Parser r e a -> r -> Int -> B.ByteString -> Result e a
runParser (ParserT f) !r (I# n) b@(B.PS (ForeignPtr _ fp) _ (I# len)) = unsafeDupablePerformIO $
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let end = plusAddr# buf len
    pure case f fp r end buf n proxy# of
      OK# _st a s n' -> let offset = minusAddr# s buf
                        in OK a (I# n') (B.drop (I# offset) b)

      Err# _st e ->  Err e
      Fail# _st  ->  Fail
{-# noinline runParser #-}
-- We mark this as noinline to allow power users to safely do unsafe state token coercions.
-- Details are discussed in https://github.com/AndrasKovacs/flatparse/pull/34#issuecomment-1326999390

-- | Run an ST based parser
runParserST :: (forall s. ParserST s r e a) -> r -> Int -> B.ByteString -> Result e a
runParserST pst !r i buf = unsafeDupablePerformIO (runParserIO pst r i buf)
{-# inlinable runParserST #-}

-- | Run an IO based parser
runParserIO :: ParserIO r e a -> r -> Int -> B.ByteString -> IO (Result e a)
runParserIO (ParserT f) !r (I# n) b@(B.PS (ForeignPtr _ fp) _ (I# len)) = do
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let end = plusAddr# buf len
    IO \st -> case f fp r end buf n st of
      OK# rw' a s n' ->  let offset = minusAddr# s buf
                         in (# rw', OK a (I# n') (B.drop (I# offset) b) #)

      Err# rw' e ->  (# rw', Err e #)
      Fail# rw'  ->  (# rw', Fail #)
{-# inlinable runParserIO #-}

-- | Run a parser on a `String` input. Reminder: @OverloadedStrings@ for `B.ByteString` does not
--   yield a valid UTF-8 encoding! For non-ASCII `B.ByteString` literal input, use `runParserS` or
--   `packUTF8` for testing.
runParserS :: Parser r e a -> r -> Int -> String -> Result e a
runParserS pa r !n s = runParser pa r n (packUTF8 s)

--------------------------------------------------------------------------------

-- | Query the `Int` state.
get :: ParserT st r e Int
get = ParserT \fp !r eob s n st -> OK# st (I# n) s n
{-# inline get #-}

-- | Write the `Int` state.
put :: Int -> ParserT st r e ()
put (I# n) = ParserT \fp !r eob s _ st -> OK# st () s n
{-# inline put #-}

-- | Modify the `Int` state.
modify :: (Int -> Int) -> ParserT st r e ()
modify f = ParserT \fp !r eob s n st ->
  case f (I# n) of
    I# n -> OK# st () s n
{-# inline modify #-}

-- | Query the environment.
ask :: ParserT st r e r
ask = ParserT \fp !r eob s n st -> OK# st r s n
{-# inline ask #-}

-- | Run a parser in a modified environment.
local :: (r -> r) -> ParserT st r e a -> ParserT st r e a
local f (ParserT g) = ParserT \fp !r eob s n st -> let !r' = f r in g fp r' eob s n st
{-# inline local #-}

--------------------------------------------------------------------------------

-- | Convert a parsing failure to a `Maybe`. If possible, use `withOption` instead.
optional :: ParserT st r e a -> ParserT st r e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# inline optional #-}

--------------------------------------------------------------------------------

-- | Parse a given `B.ByteString`. If the bytestring is statically known, consider using 'bytes' instead.
byteString :: B.ByteString -> ParserT st r e ()
byteString (B.PS (ForeignPtr bs fcontent) _ (I# len)) =

  let go64 :: Addr# -> Addr# -> Addr# -> Int# -> State# RealWorld -> Res# (State# RealWorld) e ()
      go64 bs bsend s n rw =
        let bs' = plusAddr# bs 8# in
        case gtAddr# bs' bsend of
          1# -> go8 bs bsend s n rw
#if MIN_VERSION_base(4,17,0)
          _  -> case eqWord64# (indexWord64OffAddr# bs 0#) (indexWord64OffAddr# s 0#) of
#else
          _  -> case eqWord# (indexWord64OffAddr# bs 0#) (indexWord64OffAddr# s 0#) of
#endif
            1# -> go64 bs' bsend (plusAddr# s 8#) n rw
            _  -> Fail# rw

      go8 :: Addr# -> Addr# -> Addr# -> Int# -> State# RealWorld -> Res# (State# RealWorld) e ()
      go8 bs bsend s n rw = case ltAddr# bs bsend of
#if MIN_VERSION_base(4,16,0)
        1# -> case eqWord8# (indexWord8OffAddr# bs 0#) (indexWord8OffAddr# s 0#) of
#else
        1# -> case eqWord# (indexWord8OffAddr# bs 0#) (indexWord8OffAddr# s 0#) of
#endif
          1# -> go8 (plusAddr# bs 1#) bsend (plusAddr# s 1#) n rw
          _  -> Fail# rw
        _  -> OK# rw () s n

      go :: Addr# -> Addr# -> Addr# -> Int# -> State# RealWorld -> Res# (State# RealWorld) e ()
      go bs bsend s n rw = case go64 bs bsend s n rw of
        (# rw', res #) -> case touch# fcontent rw' of
          rw'' -> (# rw'', res #)

  in ParserT \fp !r eob s n st ->
      case len <=# minusAddr# eob s of
           1# -> case runRW# (go bs (plusAddr# bs len) s n) of
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
every successful branch matching. For example, if we have @ws :: ParserT st r e ()@ for a
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

-- | Parse an ASCII `Char` for which a predicate holds. Assumption: the predicate must only return
--   `True` for ASCII-range characters. Otherwise this function might read a 128-255 range byte,
--   thereby breaking UTF-8 decoding.
satisfyASCII :: (Char -> Bool) -> ParserT st r e Char
satisfyASCII f = ParserT \fp !r eob s n st -> case eqAddr# eob s of
  1# -> Fail# st
  _  -> case derefChar8# s of
    c1 | f (C# c1) -> OK# st (C# c1) (plusAddr# s 1#) n
       | otherwise -> Fail# st
{-#  inline satisfyASCII #-}

-- | Skip an ASCII `Char` for which a predicate holds.  Assumption: the
--   predicate must only return `True` for ASCII-range characters.
satisfyASCII_ :: (Char -> Bool) -> ParserT st r e ()
satisfyASCII_ f = () <$ satisfyASCII f
{-# inline satisfyASCII_ #-}

-- | Read an `Int` from the input, as a non-empty digit sequence.
-- Fails on overflow.
readInt :: ParserT st r e Int
readInt = ParserT \fp r eob s n st -> case FlatParse.Internal.readInt eob s of
  (# (##) | #)        -> Fail# st
  (# | (# i, s' #) #) -> OK# st (I# i) s' n
{-# inline readInt #-}

-- | Read an `Int` from the input, as a non-empty case-insensitive ASCII
--   hexadecimal digit sequence.
-- Fails on overflow.
readIntHex :: ParserT st r e Int
readIntHex = ParserT \fp r eob s n st -> case FlatParse.Internal.readIntHex eob s of
  (# (##) | #)        -> Fail# st
  (# | (# i, s' #) #) -> OK# st (I# i) s' n
{-# inline readIntHex #-}

-- | Read a `Word` from the input, as a non-empty digit sequence.
-- Fails on overflow.
readWord :: ParserT st r e Int
readWord = ParserT \fp r eob s n st -> case FlatParse.Internal.readInt eob s of
  (# (##) | #)        -> Fail# st
  (# | (# i, s' #) #) -> OK# st (I# i) s' n
{-# inline readWord #-}

readWordHex :: ParserT st r e Word
readWordHex = ParserT \fp r eob s n st ->
  case FlatParse.Internal.readWordHex eob s of
    (# | (# w, s' #) #) -> OK# st (W# w) s' n
    (# (# #) | #)       -> Fail# st
{-# inline readWordHex #-}

-- | Read an `Integer` from the input, as a non-empty digit sequence.
readInteger :: ParserT st r e Integer
readInteger = ParserT \fp r eob s n st -> case FlatParse.Internal.readInteger fp eob s of
  (# (##) | #)        -> Fail# st
  (# | (# i, s' #) #) -> OK# st i s' n
{-# inline readInteger #-}

--------------------------------------------------------------------------------

-- | Run a parser zero or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
many :: ParserT st r e a -> ParserT st r e [a]
many (ParserT f) = go where
  go = ParserT \fp !r eob s n st -> case f fp r eob s n st of
    OK# st' a s n -> case runParserT# go fp r eob s n st' of
                       OK# st'' as s n -> OK# st'' (a:as) s n
                       x          -> x
    Fail# st'  -> OK# st' [] s n
    Err# st' e -> Err# st' e
{-# inline many #-}

-- | Run a parser one or more times, collect the results in a list. Note: for optimal performance,
--   try to avoid this. Often it is possible to get rid of the intermediate list by using a
--   combinator or a custom parser.
some :: ParserT st r e a -> ParserT st r e [a]
some p = (:) <$> p <*> many p
{-# inline some #-}

--------------------------------------------------------------------------------

-- | Get the current position in the input.
getPos :: ParserT st r e Pos
getPos = ParserT \fp !r eob s n st -> OK# st (addrToPos# eob s) s n
{-# inline getPos #-}

-- | Set the input position.
--
-- Warning: this can result in crashes if the position points outside the
-- current buffer. It is always safe to 'setPos' values which came from 'getPos'
-- with the current input.
setPos :: Pos -> ParserT st r e ()
setPos s = ParserT \fp !r eob _ n st -> OK# st () (posToAddr# eob s) n
{-# inline setPos #-}

-- | Return the consumed span of a parser. Use `withSpan` if possible for better efficiency.
spanOf :: ParserT st r e a -> ParserT st r e Span
spanOf (ParserT f) = ParserT \fp !r eob s n st -> case f fp r eob s n st of
  OK# st' a s' n -> OK# st' (Span (addrToPos# eob s) (addrToPos# eob s')) s' n
  x              -> unsafeCoerce# x
{-# inline spanOf #-}

-- | Bind the result together with the span of the result. CPS'd version of `spanOf`
--   for better unboxing.
withSpan :: ParserT st r e a -> (a -> Span -> ParserT st r e b) -> ParserT st r e b
withSpan (ParserT f) g = ParserT \fp !r eob s n st -> case f fp r eob s n st of
  OK# st' a s' n -> runParserT# (g a (Span (addrToPos# eob s) (addrToPos# eob s'))) fp r eob s' n st'
  x              -> unsafeCoerce# x
{-# inline withSpan #-}

-- | Return the `B.ByteString` consumed by a parser. Note: it's more efficient to use `spanOf` and
--   `withSpan` instead.
byteStringOf :: ParserT st r e a -> ParserT st r e B.ByteString
byteStringOf (ParserT f) = ParserT \fp !r eob s n st -> case f fp r eob s n st of
  OK# st' a s' n -> OK# st' (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) s' n
  x              -> unsafeCoerce# x
{-# inline byteStringOf #-}

-- | CPS'd version of `byteStringOf`. Can be more efficient, because the result is more eagerly unboxed
--   by GHC. It's more efficient to use `spanOf` or `withSpan` instead.
withByteString :: ParserT st r e a -> (a -> B.ByteString -> ParserT st r e b) -> ParserT st r e b
withByteString (ParserT f) g = ParserT \fp !r eob s n st -> case f fp r eob s n st of
  OK# st' a s' n -> runParserT# (g a (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s)))) fp r eob s' n st'
  x              -> unsafeCoerce# x
{-# inline withByteString #-}

-- | Create a `B.ByteString` from a `Span`. The result is invalid is the `Span` points
--   outside the current buffer, or if the `Span` start is greater than the end position.
unsafeSpanToByteString :: Span -> ParserT st r e B.ByteString
unsafeSpanToByteString (Span l r) =
  lookahead (setPos l >> byteStringOf (setPos r))
{-# inline unsafeSpanToByteString #-}


-- | Run a parser in a given input span. The input position and the `Int` state is restored after
--   the parser is finished, so `inSpan` does not consume input and has no side effect.  Warning:
--   this operation may crash if the given span points outside the current parsing buffer. It's
--   always safe to use `inSpan` if the span comes from a previous `withSpan` or `spanOf` call on
--   the current input.
inSpan :: Span -> ParserT st r e a -> ParserT st r e a
inSpan (Span s eob) (ParserT f) = ParserT \fp !r eob' s' n' st ->
  case f fp r (posToAddr# eob' eob) (posToAddr# eob' s) n' st of
    OK# st' a _ _ -> OK# st' a s' n'
    x             -> unsafeCoerce# x
{-# inline inSpan #-}


--------------------------------------------------------------------------------

-- | Unsafely read and return a byte from the input. It's not checked that the input is non-empty.
scanAny8# :: ParserT st r e Word8
scanAny8# = ParserT \fp !r eob s n st -> OK# st (W8# (indexWord8OffAddr# s 0#)) (plusAddr# s 1#) n
{-# inline scanAny8# #-}

-- | Decrease the current input position by the given number of bytes.
setBack# :: Int -> ParserT st r e ()
setBack# (I# i) = ParserT \fp !r eob s n st ->
  OK# st () (plusAddr# s (negateInt# i)) n
{-# inline setBack# #-}

-- | Template function, creates a @ParserT st r e ()@ which unsafely scans a given
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
            go (a:[])       = [| word8Unsafe a |]
            go []           = [| pure () |]
    _  -> case leading of

      []              -> scanw8s
      [a]             -> [| word8Unsafe a >> $scanw8s |]
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
                    [BindS (VarP (mkName "c")) (VarE 'scanAny8#),
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

      !m    =  M.fromList ((Nothing, maybe (VarE 'failed) id fallback) : branches)
      !trie = compileTrie strings
  in (m , trie)

--------------------------------------------------------------------------------

-- | Read a null-terminated bytestring (a C-style string).
--
-- Consumes the null terminator.
anyCString :: ParserT st r e B.ByteString
anyCString = ParserT \fp !r eob s n st -> go' fp eob s n st
  where
    go' fp eob s0 n st = go 0# s0 n
      where
        go n# s n = case eqAddr# eob s of
          1# -> Fail# st
          _  ->
            let s' = plusAddr# s 1#
#if MIN_VERSION_base(4,16,0)
            -- TODO below is a candidate for improving with ExtendedLiterals!
            in  case eqWord8# (indexWord8OffAddr# s 0#) (wordToWord8# 0##) of
#else
            in  case eqWord# (indexWord8OffAddr# s 0#) 0## of
#endif
                  1# -> OK# st (B.PS (ForeignPtr s0 fp) 0 (I# n#)) s' n
                  _  -> go (n# +# 1#) s' n
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
anyCStringUnsafe :: ParserT st r e B.ByteString
{-# inline anyCStringUnsafe #-}
#if MIN_VERSION_base(4,15,0)
anyCStringUnsafe = ParserT \fp !r eob s n st ->
  case eqAddr# eob s of
    1# -> Fail# st
    _  -> let n#  = cstringLength# s
              s'# = plusAddr# s (n# +# 1#)
           in OK# st (B.PS (ForeignPtr s fp) 0 (I# n#)) s'# n
#else
anyCStringUnsafe = error "Flatparse.Stateful.anyCStringUnsafe: requires GHC 9.0 / base-4.15, not available on this compiler"
#endif
