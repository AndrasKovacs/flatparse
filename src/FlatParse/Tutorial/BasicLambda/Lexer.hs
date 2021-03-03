
module FlatParse.Tutorial.BasicLambda.Lexer where

import FlatParse.Basic hiding (Parser, runParser, string, char)

import qualified FlatParse.Basic as FP
import qualified Data.ByteString as B
import Language.Haskell.TH

--------------------------------------------------------------------------------

data ParseError = ParseError !Pos !String deriving Show
type Parser = FP.Parser () ParseError

runParser :: Parser a -> B.ByteString -> Result ParseError a
runParser p = FP.runParser p ()

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

string :: String -> Q Exp
string str = [| $(string str) >> ws |]

char :: Char -> Q Exp
char c = [| $(char c) >> ws |]

pKeyword :: Parser ()
pKeyword = $(FP.switch [| case _ of
  "lam"  -> pure ()
  "let"  -> pure ()
  "in"   -> pure ()
  "if"   -> pure ()
  "then" -> pure ()
  "else" -> pure ()
  |])
