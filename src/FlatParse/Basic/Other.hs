{-# language UnboxedTuples #-}

module FlatParse.Basic.Other where

import FlatParse.Basic.Parser

-- TODO TH!!
import FlatParse.Basic.Combinators ( branch, skipBack# )
import FlatParse.Basic.Bytes ( scanBytes#, ensureBytes# )
import FlatParse.Basic.Integers ( getWord8Unsafe )

import FlatParse.Common.Trie

import GHC.Exts
import GHC.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr ( ForeignPtr(..) )
import Control.Applicative ( empty )
import qualified Data.Foldable
import Control.Monad ( forM )

import Language.Haskell.TH
import qualified Data.Map.Strict as M
import           Data.Map.Strict ( Map )

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
getCStringUnsafe = error "Flatparse.Basic.Combinators.getCStringUnsafe: requires GHC 9.0 / base-4.15, not available on this compiler"
#endif

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
