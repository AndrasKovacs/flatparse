{-# LANGUAGE UnboxedTuples #-}

module FlatParse.Common.Parser where

import GHC.Exts

-- These type aliases are used as parameters to ParserT
-- TODO 2023-01-13 raehik: document
type IOMode = State# RealWorld
type PureMode = Proxy# (##)
type STMode s = State# s
