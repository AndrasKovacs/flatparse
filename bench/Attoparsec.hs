
module Attoparsec (runSexp, runLongws, runNumcsv) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8

isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

skipWhile1 :: (Char -> Bool) -> Parser ()
skipWhile1 pred = satisfy pred *> skipWhile pred

ws, open, close, ident, sexp :: Parser ()
ws      = skipWhile (\c -> c == ' ' || c == '\n')
open    = char '(' >> ws
close   = char ')' >> ws
ident   = skipWhile1 isLatinLetter <* ws
sexp    = (open *> skipMany1 sexp <* close) <|> ident
runSexp = parseOnly sexp

longw, longws :: Parser ()
longw     = () <$ string "thisisalongkeyword"
longws    = skipMany1 (longw *> ws) <* endOfInput
runLongws = parseOnly longws

numeral, comma, numcsv :: Parser ()
numeral   = skipWhile1 (\c -> '0' <= c && c <= '9') >> ws
comma     = char ',' >> ws
numcsv    = numeral >> skipMany1 (comma >> numeral) >> endOfInput
runNumcsv = parseOnly numcsv
