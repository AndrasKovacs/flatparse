
module FlatParse.Examples.BasicLambda.Lexer where

import FlatParse.Basic hiding (Parser, runParser, string, char, cut, err)

import qualified FlatParse.Basic as FP
import qualified Data.ByteString as B
import Language.Haskell.TH

import qualified Data.Set as S

--------------------------------------------------------------------------------

data Expected
  = Lit String  -- ^ An expected literal string.
  | Msg String  -- ^ A description of what's expected.
  deriving (Eq, Show, Ord)

data Error'
  = Precise Expected     -- ^ A precisely known error, like leaving out "in" from "let"
  | Imprecise [Expected] -- ^ An imprecise error, when we expect a number of different things,
                         --   but parse something else.
  deriving Show

data Error = Error !Pos !Error'
  deriving Show

-- | Merge two errors. It is generally more helpful to prioritize printing concrete errors, and
--   suppress a deluge of "expected X, Y, Z" messages.
merge :: Error -> Error -> Error
merge err@(Error p e) err'@(Error p' e') = case (e, e') of
  (Precise _, _)                -> err   -- pick the inner concrete error
  (_, Precise _)                -> err'  -- pick the outer concrete error
  (Imprecise ss, Imprecise ss') -> Error p (Imprecise (ss ++ ss'))
   -- note: we never recover from errors, so all merged errors will in fact have exactly the same
   -- Pos. So we can simply throw away one of the two here.
{-# noinline merge #-} -- merge is "cold" code, so we shouldn't inline it.

type Parser = FP.Parser () Error

prettyError :: B.ByteString -> Error -> String
prettyError b (Error pos e) =

  let ls       = FP.lines b
      [(l, c)] = posLineCols b [pos]
      line     = if null ls then "" else ls !! l
      linum    = show l
      lpad     = map (const ' ') linum

      expected (Lit s) = show s
      expected (Msg s) = s

      err (Precise exp)    = expected exp
      err (Imprecise exps) = imprec $ S.toList $ S.fromList exps

      imprec :: [Expected] -> String
      imprec []     = error "impossible"
      imprec [s]    = expected s
      imprec (s:ss) = expected s ++ go ss where
        go []     = ""
        go [s]    = " or " ++ expected s
        go (s:ss) = ", " ++ expected s ++ go ss

  in show l ++ ":" ++ show c ++ ":\n" ++
     lpad   ++ "|\n" ++
     linum  ++ "| " ++ line ++ "\n" ++
     lpad   ++ "| " ++ replicate c ' ' ++ "^\n" ++
     "parse error: expected " ++
     err e

-- | Imprecise cut: we slap a list of expected things on inner errors.
cut :: Parser a -> [Expected] -> Parser a
cut p exps = do
  pos <- getPos
  FP.cutting p (Error pos (Imprecise exps)) merge

-- | Precise cut: we propagate at most a single expected thing.
cut' :: Parser a -> Expected -> Parser a
cut' p exp = do
  pos <- getPos
  FP.cutting p (Error pos (Precise exp)) merge

runParser :: Parser a -> B.ByteString -> Result Error a
runParser p = FP.runParser p ()

-- | Run parser, print pretty error on failure.
testParser :: Show a => Parser a -> String -> IO ()
testParser p str = case packUTF8 str of
  b -> case runParser p b of
    Err e  -> putStrLn $ prettyError b e
    OK a _ -> print a
    Fail   -> putStrLn "uncaught parse error"

lineComment :: Parser ()
lineComment =
  optioned anyWord8
    (\case 10 -> ws
           _  -> lineComment)
    (pure ())

multilineComment :: Parser ()
multilineComment = go (1 :: Int) where
  go 0 = ws
  go n = $(switch [| case _ of
    "-}" -> go (n - 1)
    "{-" -> go (n + 1)
    _    -> branch anyWord8 (go n) (pure ()) |])

ws :: Parser ()
ws = $(switch [| case _ of
  " "  -> ws
  "\n" -> ws
  "\t" -> ws
  "\r" -> ws
  "--" -> lineComment
  "{-" -> multilineComment
  _    -> pure () |])

token :: Parser a -> Parser a
token p = p <* ws
{-# inline token #-}

identStartChar :: Parser Char
identStartChar = satisfyASCII isLatinLetter
{-# inline identStartChar #-}

identChar :: Parser Char
identChar = satisfyASCII (\c -> isLatinLetter c || isDigit c)
{-# inline identChar #-}

-- | Check whether a `Span` contains exactly a keyword. Does not change parsing state.
isKeyword :: Span -> Parser ()
isKeyword span = inSpan span do
  $(FP.switch [| case _ of
      "lam"   -> pure ()
      "let"   -> pure ()
      "in"    -> pure ()
      "if"    -> pure ()
      "then"  -> pure ()
      "else"  -> pure ()
      "true"  -> pure ()
      "false" -> pure ()  |])
  eof

-- | Parse a non-keyword string.
symbol :: String -> Q Exp
symbol str = [| token $(FP.string str) |]

-- | Parser a non-keyword string, throw precise error on failure.
cutSymbol :: String -> Q Exp
cutSymbol str = [| $(symbol str) `cut'` Lit str |]

-- | Parse a keyword string.
keyword :: String -> Q Exp
keyword str = [| token ($(FP.string str) `notFollowedBy` identChar) |]

-- | Parse a keyword string, throw precise error on failure.
cutKeyword :: String -> Q Exp
cutKeyword str = [| $(keyword str) `cut'` Lit str |]
