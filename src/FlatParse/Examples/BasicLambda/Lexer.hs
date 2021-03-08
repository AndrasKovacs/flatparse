
module FlatParse.Examples.BasicLambda.Lexer where

import FlatParse.Basic hiding (Parser, runParser, string, char, cut, err)

import qualified FlatParse.Basic as FP
import qualified Data.ByteString as B
import Language.Haskell.TH

--------------------------------------------------------------------------------

data ParseError = ParseError !Pos !String deriving Show
type Parser     = FP.Parser () ParseError

-- Throw an error with current position.
err :: String -> Parser a
err msg = do
  pos <- getPos
  FP.err $ ParseError pos msg

-- | Pretty print an error.
prettyError :: B.ByteString -> ParseError -> String
prettyError b (ParseError pos msg) =
  let ls       = FP.lines b
      [(l, c)] = posLineCols b [pos]
      line     = if null ls then "" else ls !! l
      linum    = show l
      lpad     = map (const ' ') linum
  in show l ++ ":" ++ show c ++ ":\n" ++
     lpad   ++ "|\n" ++
     linum  ++ "| " ++ line ++ "\n" ++
     lpad   ++ "| " ++ replicate c ' ' ++ "^\n" ++
     "parse error: " ++ msg ++ "\n"

-- Throw an error with a given position.
errPos :: Pos -> String -> Parser a
errPos pos msg = FP.err $ ParseError pos msg

-- Cut with a given error position.
cutPos :: Parser a -> Pos -> String -> Parser a
cutPos p pos msg = FP.cut p (ParseError pos msg)

-- Cut with the starting position of the parser argument.
cut :: Parser a -> String -> Parser a
cut p msg = do
  pos <- getPos
  cutPos p pos msg

runParser :: Parser a -> B.ByteString -> Result ParseError a
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

-- | Parser a non-keyword string, throw error on failure.
cutSymbol :: String -> Q Exp
cutSymbol str = [| $(symbol str) `cut` ("expected \"" ++ str ++ "\"") |]

-- | Parse a keyword string.
keyword :: String -> Q Exp
keyword s = [| token ($(FP.string s) `notFollowedBy` identChar) |]

-- | Parse a keyword string, throw error on failure.
cutKeyword :: String -> Q Exp
cutKeyword s = [| $(keyword s) `cut` ("expected \"" ++ s ++ "\"")|]

--------------------------------------------------------------------------------
