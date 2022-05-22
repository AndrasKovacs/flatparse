{- | Compatibility layer for numeric primops.

GHC 9.2 standardized unboxed numeric primops. Prior, it was quite asymmetric.
Many primop functions used the native unboxed numerics 'Word#' and 'Int#' even
if a sized unboxed numeric was in the name, e.g. 'indexWord8OffAddr#' returning
'Word#' pre-9.2. All boxed machine integers only stored 'Word#' internally!

We target GHC 9.2's better handling. In order to maintain compatibility with
older GHCs, we define missing primops and wrap ones that changed type. Usually,
we can write a few wrappers so that 9.2 uses sized unboxed numerics everywhere,
and pre-9.2 uses native unboxed numerics everywhere. Sometimes we really want to
work with sized unboxed numerics on both, in which case we have to do more
involved primop wrapping.

The general pattern is as follows:

  * A ticked primop means it's sized on >=9.2, native on <9.2
  * A double-ticked primop means it's sized on all
  * An unticked primop should mean the same as a ticked primop (no guarantees)

Also see: https://gitlab.haskell.org/ghc/ghc/-/wikis/Unboxed-Numerics
-}

module FlatParse.Internal.UnboxedNumerics where

import GHC.Exts

-- "Switch" wrappers: sized on >=9.2, native on <9.2
byteSwap16'# :: Word16'# -> Word16'#
byteSwap32'# :: Word32'# -> Word32'#
eqWord8'# :: Word8'# -> Word8'# -> Int#
eqWord16'# :: Word16'# -> Word16'# -> Int#
eqWord32'# :: Word32'# -> Word32'# -> Int#
{-# inline byteSwap16'# #-}
{-# inline byteSwap32'# #-}
{-# inline eqWord8'#    #-}
{-# inline eqWord16'#   #-}
{-# inline eqWord32'#   #-}

-- "Sized" wrappers: sized on all
indexWord8OffAddr''# :: Addr# -> Int# -> Word8#
wordToWord8''# :: Word# -> Word8#
word8ToWord''# :: Word8# -> Word#
{-# inline indexWord8OffAddr''#  #-}
{-# inline wordToWord8''# #-}
{-# inline word8ToWord''# #-}

#if MIN_VERSION_base(4,16,0)
-- GHC >=9.2

type Word8'#  = Word8#
type Word16'# = Word16#
type Word32'# = Word32#
type Int8'#   = Int8#
type Int16'#  = Int16#
type Int32'#  = Int32#

-- "Switch" wrappers: sized on >=9.2, native on <9.2
byteSwap16'# w# = wordToWord16# (byteSwap16# (word16ToWord# w#))
byteSwap32'# w# = wordToWord32# (byteSwap32# (word32ToWord# w#))
eqWord8'# = eqWord8#
eqWord16'# = eqWord16#
eqWord32'# = eqWord32#

-- "Sized" wrappers: sized on all
indexWord8OffAddr''# = indexWord8OffAddr#
wordToWord8''# = wordToWord8#
word8ToWord''# = word8ToWord#

#else
-- GHC <9.2

type Word8'#  = Word#
type Word16'# = Word#
type Word32'# = Word#
type Int8'#   = Int#
type Int16'#  = Int#
type Int32'#  = Int#

-- "Switch" wrappers: sized on >=9.2, native on <9.2
byteSwap16'# = byteSwap16#
byteSwap32'# = byteSwap32#
eqWord8'# = eqWord#
eqWord16'# = eqWord#
eqWord32'# = eqWord#

-- No need to tick wrap these, they didn't exist <9.2
word16ToInt16# :: Word16'# -> Int#
word16ToInt16# w = narrow16Int# (word2Int# w)
word32ToInt32# :: Word32'# -> Int#
word32ToInt32# w = narrow32Int# (word2Int# w)
{-# inline word16ToInt16# #-}
{-# inline word32ToInt32# #-}

-- "Sized" wrappers: sized on all
indexWord8OffAddr''# a# i# = narrowWord8# (indexWord8OffAddr# a# i#)
wordToWord8''# = narrowWord8#
word8ToWord''# = extendWord8#

#endif

#if !MIN_VERSION_base(4,13,0)
-- GHC <8.8

type Word8# = Word#
narrowWord8# :: Word# -> Word8#
narrowWord8# = narrow8Word#
extendWord8# :: Word# -> Word8#
extendWord8# w# = w#
leWord8# :: Word8# -> Word8# -> Int#
leWord8# w1# w2# = leWord# w1# w2#

#endif
