
module Parsec (runSexp, runLongws, runNumcsv) where

import Text.Parsec
import Text.Parsec.ByteString

isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

ws, open, close, ident, sexp :: Parser ()
ws      = skipMany (satisfy \c -> c == ' ' || c == '\n')
open    = char '(' >> ws
close   = char ')' >> ws
ident   = skipMany1 (satisfy isLatinLetter) <* ws
sexp    = (open *> skipMany1 sexp <* close) <|> ident
runSexp = parse sexp ""

longw, longws :: Parser ()
longw     = () <$ string "thisisalongkeyword"
longws    = skipMany1 (longw *> ws) <* eof
runLongws = parse longws ""

numeral, comma, numcsv :: Parser ()
numeral   = skipMany1 (satisfy \c -> '0' <= c && c <= '9') >> ws
comma     = char ',' >> ws
numcsv    = numeral >> skipMany1 (comma >> numeral) >> eof
runNumcsv = parse numcsv ""
