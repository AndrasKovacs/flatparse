{- | Unsafe, highly dangerous parsing primitives using 'Addr#'.

Ensure to read the documentation before using any definitions from this module.

This module exports primitives useful for efficiently parsing binary files that
store data using an internal index.

Often, such indices describes records using a starting offset and a length.
Offsets are often relative to the file start, or some dynamic address in the
file. This way, individual records can be read out efficiently (much faster than
opening lots of small files!).

We may parse these in-place efficiently by adding record offsets to a base
memory address somewhere in the input. This is also extremely unsafe, and easy
to get catastrophically wrong. Thus, we provide as much utility as reasonable to
enable performing such parsing safely. (That means CPS functions.)

Note that all definitions here should be considered unsafe. Any 'Int#' is not
checked for positivity. You must perform any necessary checks when you obtain
your offsets and lengths as 'Int#'. Failure to do so may result in undefined
behaviour.
-}

module FlatParse.Basic.Addr where

import FlatParse.Basic.Parser
import FlatParse.Basic.Base ( takeUnsafe#, atSkipUnsafe#, lookahead )

import GHC.Exts

import qualified Data.ByteString as B

-- | Run a parser, passing it the current address the parser is at.
--
-- Useful for parsing offset-based data tables. For example, you may use this to
-- save the base address to use together with various relative offsets.
withAddr# :: (Addr# -> ParserT st e a) -> ParserT st e a
withAddr# p = ParserT \fp eob s st -> runParserT# (p s) fp eob s st
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
-- Fails if you attempt to read outside the input.
--
-- Undefined behaviour if @offset#@ or @len#@ is negative.
--
-- Name adopted from the similar-ish @indexXOffAddr#@ primops.
takeOffAddr# :: Addr# -> Int# -> Int# -> ParserT st e B.ByteString
takeOffAddr# addr# offset# len# = withOffAddr# addr# offset# (takeUnsafe# len#)
{-# inline takeOffAddr# #-}

-- | @withOffAddr# addr# offset# p@ moves to @addr#@, skips @offset#@
--   bytes, then runs the given parser @p@.
--
-- The 'Addr#' should be from 'withAddr#'.
--
-- Fails if you attempt to read outside the input.
--
-- Undefined behaviour if @offset#@ is negative.
--
-- Name adopted from the similar-ish @indexXOffAddr#@ primops.
withOffAddr# :: Addr# -> Int# -> ParserT st e a -> ParserT st e a
withOffAddr# addr# offset# =
    lookaheadFromAddr# addr# . atSkipUnsafe# offset#
{-# inline withOffAddr# #-}

-- | 'lookahead', but specify the address to lookahead from.
--
-- The 'Addr#' should be from 'withAddr#'.
lookaheadFromAddr# :: Addr# -> ParserT st e a -> ParserT st e a
lookaheadFromAddr# s = lookahead . atAddr# s
{-# inline lookaheadFromAddr# #-}

-- | Run a parser at the given address.
--
-- The 'Addr#' should be from 'withAddr#'.
--
-- This is a highly internal function -- you likely want 'lookaheadFromAddr#',
-- which will reset the address after running the parser.
atAddr# :: Addr# -> ParserT st e a -> ParserT st e a
atAddr# s (ParserT p) = ParserT \fp eob _ st -> p fp eob s st
{-# inline atAddr# #-}
