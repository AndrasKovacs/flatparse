{-# language EmptyCase, PolyKinds, KindSignatures, RankNTypes, CPP #-}
{-# options_ghc -Wno-incomplete-patterns #-}

module FlatParse.Minimal.Internal where

import GHC.Exts
import Control.Exception

#if !MIN_VERSION_base(4,15,0)
import Data.IORef
import System.IO.Unsafe
import GHC.ForeignPtr
#endif

data ParseException = ParseException deriving Show
instance Exception ParseException

{-# noinline parseError# #-}
parseError# :: forall r (b :: TYPE r). State# RealWorld -> b
parseError# = \st -> case raiseIO# (toException ParseException) st of

#if !MIN_VERSION_base(4,15,0)
{-# noinline undefinedFinalizer #-}
-- | Dummy finalizer for 'unsafeEmbedBasicIO'.
undefinedFinalizer :: ForeignPtrContents -- IORef Finalizers
undefinedFinalizer =
  PlainForeignPtr $ unsafeDupablePerformIO $
    newIORef $ error "unsafeEmbedBasicIO: attempted to build ByteString from buffer"
#endif
