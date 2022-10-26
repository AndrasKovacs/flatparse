
module FPBasic (
    runSexp
  , runLongws
  , runNumcsv) where

import FlatParse.Basic

ws      = many_ $(switch [| case _ of " " -> pure (); "\n" -> pure () |])
open    = $(getCharOf '(') >> ws
close   = $(getCharOf ')') >> ws
ident   = some_ (satisfyASCII_ isLatinLetter) >> ws
sexp    = branch open (some_ sexp >> close) ident
src     = sexp >> eof
runSexp = runParser src

longw     = $(getStringOf "thisisalongkeyword")
longws    = some_ (longw >> ws) >> eof
runLongws = runParser longws

numeral   = some_ (satisfyASCII_ isDigit) >> ws
comma     = $(getCharOf ',') >> ws
numcsv    = numeral >> many_ (comma >> numeral) >> eof
runNumcsv = runParser numcsv
