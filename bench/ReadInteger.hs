
module ReadInteger where

import FlatParse.Basic as FPBasic

readInt     = runParser FPBasic.readAsciiWord
readInteger = runParser FPBasic.readAsciiNatural
