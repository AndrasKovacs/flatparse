{-|
This module contains a simple lambda calculus parser. This parser is not optimized for maximum
performance; instead it's written in a style which emulates the look and feel of conventional
monadic parsers. An optimized implementation would use low-level `switch` expressions more often.
-}

{-# language StrictData #-}

module FlatParse.Examples.BasicLambda.Parser where

import Data.Char (ord)
import qualified Data.ByteString as B

import FlatParse.Basic hiding (Parser, runParser, string, char, cut)
import FlatParse.Examples.BasicLambda.Lexer

--------------------------------------------------------------------------------

type Name = B.ByteString

{-|
A term in the language. The precedences of different constructs are the following, in decreasing
order of strength:

* Identifiers, literals and parenthesized expressions
* Function application (left assoc)
* Multiplication (left assoc)
* Addition (left assoc)
* Equality, less-than (non-assoc)
* @lam@, @let@, @if@ (right assoc)

-}
data Tm
  = Var Name        -- ^ @x@
  | App Tm Tm       -- ^ @t u@
  | Lam Name Tm     -- ^ @lam x. t@
  | Let Name Tm Tm  -- ^ @let x = t in u@
  | BoolLit Bool    -- ^ @true@ or @false@.
  | IntLit Int      -- ^ A positive `Int` literal.
  | If Tm Tm Tm     -- ^ @if t then u else v@
  | Add Tm Tm       -- ^ @t + u@
  | Mul Tm Tm       -- ^ @t * u@
  | Eq Tm Tm        -- ^ @t == u@
  | Lt Tm Tm        -- ^ @t < u@
  deriving Show


-- | Parse an identifier. This parser uses `isKeyword` to check that an identifier is not a
--   keyword.
ident :: Parser Name
ident = token $ byteStringOf $
  spanned (identStartChar *> many_ identChar) (\_ -> fails . isKeyword)

-- | Parse an identifier, throw a precise error on failure.
cutIdent :: Parser Name
cutIdent = ident `cut'` (Msg "identifier")

digit :: Parser Int
digit = (\c -> ord c - ord '0') <$> satisfyASCII isDigit

int :: Parser Int
int = token $
  snd <$> chainr (\n (!place, !acc) -> (place*10,acc+place*n)) digit ((10,) <$> digit)

-- | Parse a literal, identifier or parenthesized expression.
atom :: Parser Tm
atom =
       (Var           <$> ident)
   <|> (BoolLit True  <$  $(keyword "true"))
   <|> (BoolLit False <$  $(keyword "false"))
   <|> (IntLit        <$> int)
   <|> ($(symbol "(") *> tm <* $(cutSymbol ")"))

expectAtom :: [Expected]
expectAtom = [Msg "identifier", Lit "true", Lit "false", Msg "parenthesized expression"]

-- | Parse an `App`-level expression.
app :: Parser Tm
app = chainl App (atom `cut` expectAtom) atom

-- | Parse a `Mul`-level expression.
mul :: Parser Tm
mul = chainl Mul app ($(symbol "*") *> app)

-- | Parse an `Add`-level expression.
add :: Parser Tm
add = chainl Add mul ($(symbol "+") *> mul)

-- | Parse an `Eq` or `Lt`-level expression.
eqLt :: Parser Tm
eqLt =
  add >>= \e1 ->
  branch $(symbol "==") (Eq e1 <$> add) $
  branch $(symbol "<")  (Lt e1 <$> add) $
  pure e1

-- | Parse a `Let`.
pLet :: Parser Tm
pLet = do
  $(keyword "let")
  x <- cutIdent
  $(cutSymbol "=")
  t <- tm
  $(cutKeyword "in")
  u <- tm
  pure $ Let x t u

-- | Parse a `Lam`.
lam :: Parser Tm
lam = do
  $(keyword "lam")
  x <- cutIdent
  $(cutSymbol ".")
  t <- tm
  pure $ Lam x t

-- | Parse an `If`.
pIf :: Parser Tm
pIf = do
  $(keyword "if")
  t <- tm
  $(cutKeyword "then")
  u <- tm
  $(cutKeyword "else")
  v <- tm
  pure $ If t u v

-- | Parse any `Tm`.
tm :: Parser Tm
tm = pLet <|> lam <|> pIf <|> eqLt

-- | Parse a complete source file.
src :: Parser Tm
src = ws *> tm <* eof `cut` (Msg "end of input" : expectAtom)


-- Examples
--------------------------------------------------------------------------------

-- testParser src p1
p1 = unlines [
  "let f = lam x. lam y. x (x (x y)) in",
  "let g = if f true then false else true in",
  "f g g h"
  ]
