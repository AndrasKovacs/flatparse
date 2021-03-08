{-# language StrictData #-}

module FlatParse.Examples.BasicLambda.Parser where

import Data.Char (ord)
import qualified Data.ByteString as B

import FlatParse.Basic hiding (Parser, runParser, string, char, err, cut)
import FlatParse.Examples.BasicLambda.Lexer

-- import qualified FlatParse.Basic as FP

--------------------------------------------------------------------------------

type Name = B.ByteString

data Tm
  = Var Name        -- x
  | App Tm Tm       -- t u
  | Lam Name Tm     -- lam x. t
  | Let Name Tm Tm  -- let x = t in u
  | BoolLit Bool    --
  | IntLit Int      --
  | If Tm Tm Tm     -- if t then u else v
  | Add Tm Tm       -- t + u
  | Mul Tm Tm       -- t * u
  | Eq Tm Tm        -- t == u
  | Lt Tm Tm        -- t < u
  deriving Show

{-
Precedences:
  4  App           (left assoc)
  3  Mul           (left assoc)
  2  Add           (left assoc)
  1  Eq,Lt         (non-assoc)
  0  Lam, Let, If  (right assoc)
-}

ident :: Parser Name
ident = token $ byteStringOf $
  spanned (identStartChar *> many_ identChar) (\_ -> fails . isKeyword)

cutIdent :: Parser Name
cutIdent = ident `cut` "expected an identifier"

digit :: Parser Int
digit = (\c -> ord c - ord '0') <$> satisfyASCII isDigit

int :: Parser Int
int = token $
  snd <$> chainr (\n (!place, !acc) -> (place*10,acc+place*n)) digit ((10,) <$> digit)

atom :: Parser Tm
atom =
       (Var           <$> ident)
   <|> (BoolLit True  <$  $(keyword "true"))
   <|> (BoolLit False <$  $(keyword "false"))
   <|> (IntLit        <$> int)
   <|> ($(symbol "(") *> tm <* $(cutSymbol ")"))

expectedAtom :: String
expectedAtom =
  "expected an identifier, \"true\", \"false\", integer literal or parenthesized term"

app :: Parser Tm
app = chainl App (atom `cut` expectedAtom) atom

mul :: Parser Tm
mul = chainl Mul app ($(symbol "*") *> app)

add :: Parser Tm
add = chainl Add mul ($(symbol "+") *> mul)

eqLt :: Parser Tm
eqLt =
  add >>= \e1 ->
  branch $(symbol "==") (Eq e1 <$> add) $
  branch $(symbol "<")  (Lt e1 <$> add) $
  pure e1

pLet :: Parser Tm
pLet = do
  $(keyword "let")
  x <- cutIdent
  $(cutSymbol "=")
  t <- tm
  $(cutKeyword "in")
  u <- tm
  pure $ Let x t u

lam :: Parser Tm
lam = do
  $(keyword "lam")
  x <- cutIdent
  $(cutSymbol ".")
  t <- tm
  pure $ Lam x t

pIf :: Parser Tm
pIf = do
  $(keyword "if")
  t <- tm
  $(cutKeyword "then")
  u <- tm
  $(cutKeyword "else")
  v <- tm
  pure $ If t u v

tm :: Parser Tm
tm = pLet <|> lam <|> pIf <|> eqLt

eofExpected :: String
eofExpected =
  "expected end of input, an identifier, \"true\", \"false\","++
  "integer literal or parenthesized term"

src :: Parser Tm
src = ws *> tm <* eof `cut` eofExpected


-- Examples
--------------------------------------------------------------------------------

-- testParser src p1
p1 = unlines [
  "let f = lam x. lam y. x in",
  "let g = if f true then false else true in",
  "f g g ."
  ]
