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
These type synonyms are used as parameters to @ParserT@. By manually threading a
state token through, we're able to unsafely embed IO actions in pure parsers
(without being removed or reordered by the compiler).

TODO 2023-01-17 raehik: perhaps more/better explanation?
-}
type PureMode = Proxy# Void
type IOMode   = State# RealWorld
type STMode s = State# s
