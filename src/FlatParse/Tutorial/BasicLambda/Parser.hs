{-# language StrictData #-}

module FlatParse.Tutorial.BasicLambda.Parser where

import qualified Data.ByteString as B

-- import FlatParse.Basic hiding (Parser, runParser, string, char)
-- import qualified FlatParse.Basic as FP
-- import FlatParse.Tutorial.BasicLambda.Lexer
-- import Data.Reflection

-- type Name = B.ByteString

-- data Tm r
--   = Var Name        -- x
--   | App Tm Tm       -- t u
--   | Lam Name Tm Tm  -- lam x. t
--   | Let Name Tm Tm  -- let x = t in u
--   | BoolLit Bool    --
--   | IntLit Int      --
--   | If Tm Tm Tm     -- if t then u else v
--   | Add Tm Tm       -- t + u
--   | Mul Tm Tm       -- t * u
--   | Eq Tm Tm        -- t == u
--   | Lt Tm Tm        -- t < u


-- {-
-- Precedences in decreasing order of strength:
--   0  App           (left assoc)
--   1  Mul           (left assoc)
--   2  Add           (left assoc)
--   3  Eq,Lt         (non-assoc)
--   4  Lam, Let, If  (right assoc)
-- -}


-- -- identStartChar = satisfyASCII isLatinLetter
-- -- identChar = satisfyASCII (\c -> isLatinLetter c || isDigit c)

-- -- pIdent :: Parser Name
-- -- pIdent = asByteString (
-- --       (pKeyword >> some_ identChar)
-- --   <|> (identStartChar >> many_ identChar))

--   -- p <- getPos
--   -- (pKeyword >> finishName p) <|> (identStart >> many
