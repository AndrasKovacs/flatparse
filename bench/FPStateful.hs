
module FPStateful (
    runSexp
  , runLongws
  , runNumcsv) where

import FlatParse.Stateful

ws      = many_ $(switch [| case _ of " " -> pure (); "\n" -> pure () |])
open    = $(char '(') >> ws
close   = $(char ')') >> ws
ident   = some_ (satisfyASCII isLatinLetter) >> ws
sexp    = branch open (some_ sexp >> close) ident
src     = sexp >> eof
runSexp = runParser src 0 0

longw     = $(string "thisisalongkeyword")
longws    = some_ (longw >> ws) >> eof
runLongws = runParser longws 0 0

numeral   = some_ (satisfyASCII isDigit) >> ws
comma     = $(char ',') >> ws
numcsv    = numeral >> many_ (comma >> numeral) >> eof
runNumcsv = runParser numcsv 0 0
