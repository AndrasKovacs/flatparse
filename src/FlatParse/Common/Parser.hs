module FlatParse.Common.Parser where

import GHC.Exts
import Data.Void ( Void )

-- These type aliases are used as parameters to ParserT
-- TODO 2023-01-13 raehik: document
type IOMode = State# RealWorld
--type PureMode = Proxy# (##)
-- TODO using empty unboxed tuple type forces UnboxedTuples everywhere :D
type PureMode = Proxy# Void
type STMode s = State# s
