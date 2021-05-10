
module ReadInteger where

import FlatParse.Basic as FPBasic

readInt     = runParser FPBasic.readInt
readInteger = runParser FPBasic.readInteger
