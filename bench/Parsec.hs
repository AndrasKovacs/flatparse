
module Parsec (runSexp, runLongws, runNumcsv, runTm) where

import Control.Monad
import Text.Parsec hiding (chainl, digit)
import Text.Parsec.ByteString
import qualified Data.ByteString as B
import Data.Char hiding (isDigit)
import Common

isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

ws, open, close, ident, sexp :: Parser ()
ws      = skipMany (satisfy \c -> c == ' ' || c == '\n')
open    = char '(' >> ws
close   = char ')' >> ws
ident   = skipMany1 (satisfy isLatinLetter) <* ws
sexp    = (open *> skipMany1 sexp <* close) <|> ident
runSexp = parse sexp ""

byteString :: B.ByteString -> Parser ()
byteString b = do
  i <- getInput
  guard $ B.isPrefixOf b i
  setInput $ B.drop (B.length b) i

longw, longws :: Parser ()
longw     = () <$ byteString "thisisalongkeyword"
longws    = skipMany1 (longw *> ws) <* eof
runLongws = parse longws ""

numeral, comma, numcsv :: Parser ()
numeral   = skipMany1 (satisfy \c -> '0' <= c && c <= '9') >> ws
comma     = char ',' >> ws
numcsv    = numeral >> skipMany1 (comma >> numeral) >> eof
runNumcsv = parse numcsv ""

--------------------------------------------------------------------------------

{-# inline byteStringOf #-}
byteStringOf :: Parser a -> Parser B.ByteString
byteStringOf p = do
  i <- getInput
  _ <- p
  i' <- getInput
  pure $! B.take (B.length i - B.length i') i

ident' :: Parser B.ByteString
ident' = byteStringOf (skipMany1 (satisfy \c -> isLatinLetter c || isDigit c)) <* ws

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

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

decimal :: Parser Int
decimal = chainl (\x n -> 10*x + n) digit digit

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
tm =  (do {byteString "fun"; ws; x <- ident'; dot; t <- tm; pure (Lam x t)})
  <|> (do {byteString "let"; ws; x <- ident'; equal; t <- tm; semi; u <- tm; pure (Let x t u)})
  <|> add

runTm = parse (ws *> tm <* eof) ""
