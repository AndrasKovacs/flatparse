
module Megaparsec (runSexp, runLongws, runNumcsv) where

import Control.Applicative
import qualified Data.ByteString as B
import Text.Megaparsec
import Data.Char

type Parser = Parsec () B.ByteString

isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

char8 :: Char -> Parser ()
char8 c = () <$ single (fromIntegral (ord c))

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile f = () <$ takeWhileP Nothing (f . chr . fromIntegral)

skipWhile1 :: (Char -> Bool) -> Parser ()
skipWhile1 f = () <$ takeWhile1P Nothing (f . chr . fromIntegral)

ws, open, close, ident, sexp :: Parser ()
ws      = skipWhile (\c -> c == ' ' || c == '\n')
open    = char8 '(' >> ws
close   = char8 ')' >> ws
ident   = skipWhile1 isLatinLetter <* ws
sexp    = (open *> skipSome sexp <* close) <|> ident
runSexp = runParser sexp ""

longw, longws :: Parser ()
longw     = () <$ chunk "thisisalongkeyword"
longws    = skipSome (longw *> ws) <* eof
runLongws = runParser longws ""

numeral, comma, numcsv :: Parser ()
numeral   = skipWhile1 (\c -> '0' <= c && c <= '9') >> ws
comma     = single (fromIntegral (ord ',')) >> ws
numcsv    = numeral >> skipSome (comma >> numeral) >> eof
runNumcsv = runParser numcsv ""
