-- | Common low-level parser definitions.

module FlatParse.Common.Parser
  (
  -- * Parser state token types
  -- $parser-state-token-types
    type PureMode
  , type IOMode
  , type STMode
  ) where

import GHC.Exts
import Data.Void ( Void )

{- $parser-state-token-types
These type synonyms are used as parameters to @ParserT@. Different state tokens
support different embedded effects.
-}

-- TODO 2023-01-17 raehik: perhaps more/better explanation?

type PureMode = Proxy# Void
type IOMode   = State# RealWorld
type STMode s = State# s
