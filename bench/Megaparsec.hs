
module Megaparsec (runSexp, runLongws, runNumcsv, runTm) where

import Control.Applicative
import qualified Data.ByteString as B
import Text.Megaparsec
import Text.Megaparsec.Byte.Lexer
import Data.Char
import Common

type Parser = Parsec () B.ByteString

isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

char8 :: Char -> Parser ()
char8 c = () <$ single (fromIntegral (ord c))

{-# inline skipWhile #-}
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile f = () <$ takeWhileP Nothing (f . chr . fromIntegral)

{-# inline skipWhile1 #-}
skipWhile1 :: (Char -> Bool) -> Parser ()
skipWhile1 f = () <$ takeWhile1P Nothing (f . chr . fromIntegral)

{-# inline takeWhile1 #-}
takeWhile1 :: (Char -> Bool) -> Parser B.ByteString
takeWhile1 f = takeWhile1P Nothing (f . chr . fromIntegral)

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

--------------------------------------------------------------------------------

ident' :: Parser B.ByteString
ident' = takeWhile1 (\c -> isLatinLetter c || isDigit c) <* ws

equal = char8 '=' >> ws
semi  = char8 ';' >> ws
dot   = char8 '.' >> ws
addOp = char8 '+' >> ws
mulOp = char8 '*' >> ws
parl  = char8 '(' >> ws
parr  = char8 ')' >> ws

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
tm =  (do {_ <- chunk "fun"; ws; x <- ident'; dot; t <- tm; pure (Lam x t)})
  <|> (do {_ <- chunk "let"; ws; x <- ident'; equal; t <- tm; semi; u <- tm; pure (Let x t u)})
  <|> add

runTm = runParser (ws *> tm <* eof) ""
