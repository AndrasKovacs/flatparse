{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-} -- needed for manual ZeroBitType def (unsure why)

module FlatParse.Common.GHCExts
  ( module FlatParse.Common.GHCExts
  , module GHC.Exts
  ) where

import GHC.Exts

#if !MIN_VERSION_base(4,17,0)
-- TODO 2023-01-13 raehik: document: which GHC version, why?
type ZeroBitRep = 'TupleRep ('[] :: [RuntimeRep])
type ZeroBitType = TYPE ZeroBitRep
#endif
