
module FPStateful (
    runSexp
  , runLongws
  , runNumcsv) where

import FlatParse.Stateful
import FlatParse.Common.Assorted

ws, open, close, ident, sexp, src :: Parser () () ()
ws      = skipMany $(switch [| case _ of " " -> pure (); "\n" -> pure () |])
open    = $(char '(') >> ws
close   = $(char ')') >> ws
ident   = skipSome (satisfyASCII_ isLatinLetter) >> ws
sexp    = branch open (skipSome sexp >> close) ident
src     = sexp >> eof
runSexp = runParser src () 0

longw, longws :: Parser () () ()
longw     = $(string "thisisalongkeyword")
longws    = skipSome (longw >> ws) >> eof
runLongws = runParser longws () 0

numeral, comma, numcsv :: Parser () () ()
numeral   = skipSome (satisfyASCII_ isDigit) >> ws
comma     = $(char ',') >> ws
numcsv    = numeral >> skipMany (comma >> numeral) >> eof
runNumcsv = runParser numcsv () 0
