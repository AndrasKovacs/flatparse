{-# language UnboxedTuples #-}

module FlatParse.Basic.Integers.Varints where

import FlatParse.Basic.Parser

import FlatParse.Common.Assorted ( shortInteger )
import FlatParse.Common.Numbers ( getVarintProtobuf# )

import GHC.Exts

getVarintProtobuf :: Parser e Int
getVarintProtobuf = Parser \fp eob s ->
    case getVarintProtobuf# eob s of
      (# (##) | #) -> Fail#
      (# | (# i#, s#, _n# #) #) -> OK# (I# i#) s#
{-# inline getVarintProtobuf #-}

getVarintProtobufInteger :: Parser e Integer
getVarintProtobufInteger = Parser \fp eob s ->
    case getVarintProtobuf# eob s of
      (# (##) | #) -> Fail#
      (# | (# i#, s#, n# #) #) ->
        case n# >=# 62# of -- TODO unsure if 62 or 63
          1# -> error "TODO overflow"
          _  -> OK# (shortInteger i#) s#
{-# inline getVarintProtobufInteger #-}
