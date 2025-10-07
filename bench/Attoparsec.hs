
module Attoparsec (runSexp, runLongws, runNumcsv, runTm) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Common
import qualified Data.ByteString as B

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


--------------------------------------------------------------------------------

ident' :: Parser B.ByteString
ident' = takeWhile1 (\c -> isLatinLetter c || isDigit c) <* ws

equal = char '=' >> ws
semi  = char ';' >> ws
dot   = char '.' >> ws
addOp = char '+' >> ws
mulOp = char '*' >> ws
parl  = char '(' >> ws
parr  = char ')' >> ws

chainl :: (b -> a -> b) -> Parser b -> Parser a -> Parser b
chainl f start elem = start >>= go where
  go b = do {!a <- elem; go $! f b a} <|> pure b
{-# inline chainl #-}

add :: Parser Tm
add = chainl Add mul (addOp *> mul)

mul :: Parser Tm
mul = chainl Mul spine (mulOp *> spine)

spine :: Parser Tm
spine = chainl App atom atom

atom :: Parser Tm
atom =
        (Int <$> (decimal <* ws))
    <|> (Var <$> ident')
    <|> (parl *> tm <* parr)

tm :: Parser Tm
tm =  (do {_ <- string "fun"; ws; x <- ident'; dot; t <- tm; pure (Lam x t)})
  <|> (do {_ <- string "let"; ws; x <- ident'; equal; t <- tm; semi; u <- tm; pure (Let x t u)})
  <|> add

runTm = parseOnly (ws *> tm <* endOfInput)
