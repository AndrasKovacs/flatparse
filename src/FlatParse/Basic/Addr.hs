{- | Highly dangerous parsing primitives using 'Addr#'.

Ensure to read the documentation before using any definitions from this module.

This module exports primitives useful for efficiently parsing binary files that
store data using an internal index.

Often, such indices describes records using a starting offset and a length.
Offsets are often relative to the file start, or some dynamic address in the
file. This way, individual records can be read out efficiently (much faster than
opening lots of small files!).

We may parse these in-place extremely efficiently by adding record offsets to a
base memory address somewhere in the parsing data. This is also extremely
unsafe, and easy to get catastrophically wrong. Thus, we provide as much utility
as reasonable to enable performing such parsing safely. (That means CPS
functions.)
-}

module FlatParse.Basic.Addr where

import FlatParse.Basic.Parser
import FlatParse.Basic.Internal

import GHC.Exts

import qualified Data.ByteString as B

-- | Run a parser, passing it the current address the parser is at.
--
-- Useful for parsing offset-based data tables. For example, you may use this to
-- save the base address to use together with various relative offsets.
withAddr# :: (Addr# -> Parser e a) -> Parser e a
withAddr# p = Parser \fp eob s -> runParser# (p s) fp eob s
{-# inline withAddr# #-}

-- | @takeOffAddr# addr# offset# len#@ moves to @addr#@, skips @offset#@
--   bytes, reads @len#@ bytes into a 'ByteString', and restores the original
--   address.
--
-- The 'Addr#' should be from 'withAddr#'.
--
-- Useful for parsing offset-based data tables. Ex: Your file contains an index
-- storing @(OFFSET, LENGTH)@ entries where the offset is the byte position in
-- the file. Begin with @'withAddr#' $ \tableBase# -> ...@, then read each entry
-- like @'takeOffAddr#' tableBase# OFFSET LENGTH@.
--
-- Fails if you attempt to read outside the parsing data.
--
-- Name adopted from the similar-ish @indexXOffAddr#@ primops.
takeOffAddr# :: Addr# -> Int# -> Int# -> Parser e B.ByteString
takeOffAddr# addr# offset# len# = withOffAddr# addr# offset# (take# len#)
{-# inline takeOffAddr# #-}

-- | @withOffAddr# addr# offset# p@ moves to @addr#@, skips @offset#@
--   bytes, then runs the given parser @p@.
--
-- The 'Addr#' should be from 'withAddr#'.
--
-- Fails if you attempt to read outside the parsing data.
--
-- Name adopted from the similar-ish @indexXOffAddr#@ primops.
withOffAddr# :: Addr# -> Int# -> Parser e a -> Parser e a
withOffAddr# addr# offset# =
    lookaheadFromAddr# addr# . atSkip# offset#
{-# inline withOffAddr# #-}

-- | 'lookahead', but specify the address to lookahead from.
--
-- The 'Addr#' should be from 'withAddr#'.
lookaheadFromAddr# :: Addr# -> Parser e a -> Parser e a
lookaheadFromAddr# s = lookahead . atAddr# s
{-# inline lookaheadFromAddr# #-}

-- | Run a parser at the given address.
--
-- The 'Addr#' should be from 'withAddr#'.
--
-- This is a highly internal function -- you likely want 'lookaheadFromAddr#',
-- which will reset the address after running the parser.
atAddr# :: Addr# -> Parser e a -> Parser e a
atAddr# s (Parser p) = Parser \fp eob _ -> p fp eob s
{-# inline atAddr# #-}
