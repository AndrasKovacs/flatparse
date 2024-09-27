{-# language Strict, ViewPatterns #-}

module Flatparse.Stateful.Batteries where

import qualified Data.ByteString as B
import qualified FlatParse.Stateful as FP
import qualified Data.Set as S
import Language.Haskell.TH

type Parser = FP.Parser Int Error

data Expected
  = Lit String
  | Msg String
  | ExactIndent Int
  | IndentMore Int
  deriving (Eq, Show, Ord)

data Error
  = Precise FP.Pos Expected
  | Imprecise FP.Pos [Expected]
  deriving Show

errorPos :: Error -> FP.Pos
errorPos = \case
  Precise p _     -> p
  Imprecise p _   -> p

-- | Merge two errors. Inner errors (which were thrown at points with more consumed inputs)
--   are preferred. If errors are thrown at identical input positions, we prefer precise errors
--   to imprecise ones.
--
--   The point of prioritizing inner and precise errors is to suppress the deluge of "expected"
--   items, and instead try to point to a concrete issue to fix.
merge :: Error -> Error -> Error
merge e e' = case (errorPos e, errorPos e') of
  (p, p') | p < p' -> e'
  (p, p') | p > p' -> e
  (p, p')          -> case (e, e') of
    (Precise{}      , _               ) -> e
    (_              , Precise{}       ) -> e'
    (Imprecise _ es , Imprecise _ es' ) -> Imprecise p (es ++ es')
{-# noinline merge #-} -- cold code


-- | Pretty print an error. The `B.ByteString` input is the source file. The offending line from the
--   source is displayed in the output.
prettyError :: B.ByteString -> Error -> String
prettyError b e =

  let pos :: FP.Pos
      pos      = case e of Imprecise pos e -> pos
                           Precise pos e   -> pos
      ls       = FP.linesUtf8 b
      (l, c)   = head $ FP.posLineCols b [pos]
      line     = if l < length ls then ls !! l else ""
      linum    = show l
      lpad     = map (const ' ') linum

      expected (Lit s) = show s
      expected (Msg s) = s
      expected (ExactIndent col) = "expected a token indented to column " ++ show (col + 1)
      expected (IndentMore col) = "expected a token indented to column " ++ show (col + 1) ++ " or more."

      err (Precise _ e)    = expected e
      err (Imprecise _ es) = imprec $ S.toList $ S.fromList es

      imprec :: [Expected] -> String
      imprec []     = error "impossible"
      imprec [e]    = expected e
      imprec (e:es) = expected e ++ go es where
        go []     = ""
        go [e]    = " or " ++ expected e
        go (e:es) = ", " ++ expected e ++ go es

  in show l ++ ":" ++ show c ++ ":\n" ++
     lpad   ++ "|\n" ++
     linum  ++ "| " ++ line ++ "\n" ++
     lpad   ++ "| " ++ replicate c ' ' ++ "^\n" ++
     "parse error: expected " ++ err e

{-
Batteries included for potentially indentation-sensitive parsing

token parsing scheme:
  1 check indentation
  2 read thing
  3 read ws

input
  - parser for first ident character
  - parser for rest ident character
  - parser for whitespace
  - parser for line comment, multiline open, multiline close, whether multiline is nestable
  - list of keywords

th-generated output
  - ident parser, handles keywords, indentation, returns span
    + strict version
  - symbol parser, handles keywords, idents, indentation, return span
    + strict version
  - case split:
      - handles keywords, ident overlaps, indentation, returns span

non-th exports
  - all the useful stuff
  - error types, printing, handling

-}

getPos :: Parser FP.Pos
getPos = FP.getPos
{-# inline getPos #-}

-- | Imprecise cut: we slap a list of expected things on inner errors.
cut :: Parser a -> [Expected] -> Parser a
cut p exps = do
  pos <- getPos
  FP.cutting p (Imprecise pos exps) merge
{-# inline cut #-}

-- | Precise cut: we propagate at most a single expected thing.
pcut :: Parser a -> Expected -> Parser a
pcut p exp = do
  pos <- getPos
  FP.cutting p (Precise pos exp) merge

runParser :: Parser a -> B.ByteString -> FP.Result Error a
runParser p src = FP.runParser p 0 0 src

-- | Run parser, print pretty error on failure.
testParser :: Show a => Parser a -> String -> IO ()
testParser p (FP.strToUtf8 -> str) = case runParser p str of
    FP.Err e    -> putStrLn $ prettyError str e
    FP.OK a _ _ -> print a
    FP.Fail     -> putStrLn "parse error"

-- | Query the current indentation level, fail if it's smaller than the current expected level.
lvl :: Parser Int
lvl = do
  lvl <- FP.ask
  currentLvl <- FP.get
  if currentLvl < lvl
    then FP.empty
    else pure currentLvl
{-# inline lvl #-}

lvl' :: Parser Int
lvl' = do
  lvl <- FP.ask
  currentLvl <- FP.get
  if currentLvl < lvl
    then
    else pure currentLvl
{-# inline lvl' #-}

--------------------------------------------------------------------------------

data Config = Config
  (CodeQ (Parser Char))  -- ident start char
  (CodeQ (Parser Char))  -- ident nonstart char
  String                 -- set of whitespace characters
  String                 -- line comment start
  String                 -- block comment start
  String                 -- block comment end
  [String]               -- keywords

chargeBatteries :: Config -> Q [Dec]
chargeBatteries (Config identStart identRest wsChars lineComment
                        blockCommentStart blockCommentEnd keywords) = do


  undefined


--------------------------------------------------------------------------------
