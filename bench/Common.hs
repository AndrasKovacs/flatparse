{-# language Strict #-}

module Common where

import qualified Data.ByteString as B

type Name = B.ByteString
data Tm = Var Name | App Tm Tm | Lam Name Tm | Let Name Tm Tm | Int Int | Add Tm Tm | Mul Tm Tm
  deriving Show
