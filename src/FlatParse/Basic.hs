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
import FlatParse.Common.Trie

import FlatParse.Basic.Parser
import FlatParse.Basic.Integers
import FlatParse.Basic.Internal
import FlatParse.Basic.Bytes
import FlatParse.Basic.Strings
import FlatParse.Basic.Position
import FlatParse.Basic.Addr

import Control.Applicative

import Control.Monad
import Data.Foldable
import Data.Map (Map)
import GHC.Exts
import GHC.Word
import GHC.ForeignPtr ( ForeignPtr(..) )
import Language.Haskell.TH
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import qualified Data.Map.Strict as M

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
      fallback rule n = [| skipBack# n >> $(pure $ VarE $ fst $ ix branches rule) |]

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
