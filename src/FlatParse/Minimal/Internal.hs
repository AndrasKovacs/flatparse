{-# language EmptyCase, PolyKinds, KindSignatures, RankNTypes #-}
{-# options_ghc -Wno-incomplete-patterns #-}

module FlatParse.Minimal.Internal where

import GHC.Exts
import Control.Exception
import Data.IORef
import System.IO.Unsafe
import GHC.ForeignPtr

data ParseException = ParseException deriving Show
instance Exception ParseException

{-# noinline parseError# #-}
parseError# :: forall r (b :: TYPE r). State# RealWorld -> b
parseError# = \st -> case raiseIO# (toException ParseException) st of

{-# noinline undefinedFinalizer #-}
-- | Dummy finalizer for 'unsafeEmbedBasicIO'.
undefinedFinalizer :: ForeignPtrContents -- IORef Finalizers
undefinedFinalizer =
  PlainForeignPtr $ unsafeDupablePerformIO $
    newIORef $ error "unsafeEmbedBasicIO: attempted to build ByteString from buffer"
