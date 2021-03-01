
module FP (
    runSexp
  , runLongws
  , runNumcsv) where

import FlatParse

ws      = many_ $(switch [| case _ of " " -> pure (); "\n" -> pure () |])
open    = $(char '(') >> ws
close   = $(char ')') >> ws
ident   = some_ (satisfyA isLatinLetter) >> ws
sexp    = br open (some_ sexp >> close) ident
src     = sexp >> eof
runSexp = runParser src ()

longw     = $(string "thisisalongkeyword")
longws    = some_ (longw >> ws) >> eof
runLongws = runParser longws ()

numeral   = some_ (satisfyA \c -> '0' <= c && c <= '9') >> ws
comma     = $(char ',') >> ws
numcsv    = numeral >> many_ (comma >> numeral) >> eof
runNumcsv = runParser numcsv ()
