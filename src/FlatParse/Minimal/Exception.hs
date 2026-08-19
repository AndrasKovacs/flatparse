{-# language EmptyCase, PolyKinds, KindSignatures, RankNTypes #-}
{-# options_ghc -Wno-incomplete-patterns #-}

module FlatParse.Minimal.Exception where

import GHC.Exts
import Control.Exception

data ParseException = ParseException deriving Show
instance Exception ParseException

{-# noinline parseError# #-}
parseError# :: forall r (b :: TYPE r). State# RealWorld -> b
parseError# = \st -> case raiseIO# (toException ParseException) st of
