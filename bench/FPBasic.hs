
module FPBasic (
    runSexp
  , runLongws
  , runNumcsv) where

import FlatParse.Basic
import FlatParse.Common.Assorted

ws      = skipMany $(switch [| case _ of " " -> pure (); "\n" -> pure () |])
open    = $(char '(') >> ws
close   = $(char ')') >> ws
ident   = skipSome (satisfyAscii_ isLatinLetter) >> ws
sexp    = branch open (skipSome sexp >> close) ident
src     = sexp >> eof
runSexp = runParser src

longw     = $(string "thisisalongkeyword")
longws    = skipSome (longw >> ws) >> eof
runLongws = runParser longws

numeral   = skipSome (satisfyAscii_ isDigit) >> ws
comma     = $(char ',') >> ws
numcsv    = numeral >> skipMany (comma >> numeral) >> eof
runNumcsv = runParser numcsv
