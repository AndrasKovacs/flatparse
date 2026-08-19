-- | 'GHC.Exts' compatibility wrapper.

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-} -- needed for manual ZeroBitType def (unsure why)
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module FlatParse.Common.GHCExts
  ( module FlatParse.Common.GHCExts
  , module GHC.Exts
  ) where

#if __GLASGOW_HASKELL__ == 912
import GHC.Exts hiding (BCO, mkApUpd0#, newBCO#)
#else
import GHC.Exts
#endif

#if !MIN_VERSION_base(4,17,0)
{-
GHC 9.4 clarified the story for types without runtime representations. These
type synonyms are defined and used to simplify certain internal definitions
(e.g. 'State#'). They are nicer than using the "expanded" type, so we define
them here for older compilers.
-}
type ZeroBitRep = 'TupleRep ('[] :: [RuntimeRep])
type ZeroBitType = TYPE ZeroBitRep
#endif
