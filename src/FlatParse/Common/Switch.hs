module FlatParse.Common.Switch where

import Control.Monad (forM)
import Data.Foldable (foldl')
import Data.Map (Map)
import Language.Haskell.TH

import qualified Data.Map.Strict as M

import FlatParse.Common.Assorted

-- Switch trie compilation
--------------------------------------------------------------------------------

data Trie a = Branch !a !(Map Word (Trie a))
  deriving Show

type Rule = Maybe Int

nilTrie :: Trie Rule
nilTrie = Branch Nothing mempty

updRule :: Int -> Maybe Int -> Maybe Int
updRule rule = Just . maybe rule (min rule)

insert :: Int -> [Word] -> Trie Rule -> Trie Rule
insert rule = go where
  go [] (Branch rule' ts) =
    Branch (updRule rule rule') ts
  go (c:cs) (Branch rule' ts) =
    Branch rule' (M.alter (Just . maybe (go cs nilTrie) (go cs)) c ts)

listToTrie :: [(Int, String)] -> Trie Rule
listToTrie = foldl' (\t (!r, !s) -> insert r (charToBytes =<< s) t) nilTrie

-- | Decorate a trie with the minimum lengths of non-empty paths. This
--   is used later to place `ensure`.
mindepths :: Trie Rule -> Trie (Rule, Int)
mindepths (Branch rule ts) =
  if M.null ts then
    Branch (rule, 0) mempty
  else
    let !ts' = M.map mindepths ts
        !min = minimum (M.map (\(Branch (rule,d) _) -> maybe (d + 1) (\_ -> 1) rule) ts') in
    Branch (rule, min) ts'

data Trie' a
  = Branch' !a !(Map Word (Trie' a))
  | Path !a ![Word] !(Trie' a)
  deriving Show

-- | Compress linear paths.
pathify :: Trie (Rule, Int) -> Trie' (Rule, Int)
pathify (Branch a ts) = case M.toList ts of
  [] -> Branch' a mempty
  [(w, t)] -> case pathify t of
           Path (Nothing, _) ws t -> Path a (w:ws) t
           t                      -> Path a [w] t
  _   -> Branch' a (M.map pathify ts)

-- | Compute where to fall back after we exhausted a branch. If the branch is
--   empty, that means we've succeded at reading and we jump to the rhs rule.
fallbacks :: Trie' (Rule, Int) -> Trie' (Rule, Int, Int)
fallbacks = go Nothing 0  where
  go :: Rule -> Int -> Trie' (Rule, Int) -> Trie' (Rule, Int, Int)
  go !rule !n (Branch' (rule', d) ts)
    | M.null ts        = Branch' (rule', 0, d) mempty
    | Nothing <- rule' = Branch' (rule, n, d) (go rule (n + 1) <$> ts)
    | otherwise        = Branch' (rule', 0, d) (go rule' 1     <$> ts)
  go rule n (Path (rule', d) ws t)
    | Nothing <- rule' = Path (rule, n, d)  ws (go rule (n + length ws) t)
    | otherwise        = Path (rule', 0, d) ws (go rule' (length ws) t)

-- | Decorate with `ensure` invocations, represented as
--   `Maybe Int`.
ensureBytes :: Trie' (Rule, Int, Int) -> Trie' (Rule, Int, Maybe Int)
ensureBytes = go 0 where
  go :: Int -> Trie' (Rule, Int, Int) -> Trie' (Rule, Int, Maybe Int)
  go !res = \case
    Branch' (r, n, d) ts
      | M.null ts -> Branch' (r, n, Nothing) mempty
      | res < 1   -> Branch' (r, n, Just d ) (go (d   - 1) <$> ts)
      | otherwise -> Branch' (r, n, Nothing) (go (res - 1) <$> ts)
    Path (r, n, d) ws t -> case length ws of
      l | res < l   -> Path (r, n, Just d ) ws (go (d   - l) t)
        | otherwise -> Path (r, n, Nothing) ws (go (res - l) t)

compileTrie :: [(Int, String)] -> Trie' (Rule, Int, Maybe Int)
compileTrie = ensureBytes . fallbacks . pathify . mindepths . listToTrie

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

makeRawSwitch :: [(String, Q Exp)] -> Maybe (Q Exp) -> Q Exp
makeRawSwitch branches deflt = do
  branches <- forM branches $ \(s, body) -> do
    body <- body
    pure $ Match (LitP (StringL s)) (NormalB body) []
  branches <- case deflt of
    Nothing    -> pure branches
    Just deflt -> do
      deflt <- deflt
      pure $ branches ++ [Match WildP (NormalB deflt) []]
  pure $ CaseE (UnboundVarE (mkName "_")) branches
