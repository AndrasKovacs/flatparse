
module ReadInteger where

import FlatParse.Basic as FPBasic

readInt     = runParser FPBasic.getAsciiDecimalInt
readInteger = runParser FPBasic.getAsciiDecimalInteger
