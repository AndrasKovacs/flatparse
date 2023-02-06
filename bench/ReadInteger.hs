
module ReadInteger where

import FlatParse.Basic as FPBasic

readInt     = runParser FPBasic.anyAsciiDecimalInt
readInteger = runParser FPBasic.anyAsciiDecimalInteger
