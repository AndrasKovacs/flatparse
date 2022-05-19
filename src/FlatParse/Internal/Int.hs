{- | Compatibility layer for numeric primops.

GHC 9.2 standardized unboxed numeric primops. Prior, it was quite asymmetric.
Many primop functions used the native unboxed numerics 'Word#' and 'Int#' even
if a sized unboxed numeric was in the name, e.g. 'indexWord8OffAddr#' returning
'Word#' pre-9.2.

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

module FlatParse.Internal.Int where

import GHC.Exts

#if MIN_VERSION_base(4,16,0)
-- GHC >=9.2

-- "Switch" wrappers: sized on >=9.2, native on <9.2
byteSwap16'# :: Word16# -> Word16#
byteSwap16'# w# = wordToWord16# (byteSwap16# (word16ToWord# w#))
byteSwap32'# :: Word32# -> Word32#
byteSwap32'# w# = wordToWord32# (byteSwap32# (word32ToWord# w#))
eqWord8'# :: Word8# -> Word8# -> Int#
eqWord8'# = eqWord8#
eqWord16'# :: Word16# -> Word16# -> Int#
eqWord16'# = eqWord16#
eqWord32'# :: Word32# -> Word32# -> Int#
eqWord32'# = eqWord32#

-- "Sized" wrappers: sized on all
indexWord8OffAddr''# :: Addr# -> Int# -> Word8#
indexWord8OffAddr''# = indexWord8OffAddr#
wordToWord8''# :: Word# -> Word8#
wordToWord8''# = wordToWord8#
word8ToWord''# :: Word8# -> Word#
word8ToWord''# = word8ToWord#

#else
-- GHC <9.2

-- "Switch" wrappers: sized on >=9.2, native on <9.2
byteSwap16'# :: Word# -> Word#
byteSwap16'# = byteSwap16#
byteSwap32'# :: Word# -> Word#
byteSwap32'# = byteSwap32#
eqWord8'# :: Word# -> Word# -> Int#
eqWord8'# = eqWord#
eqWord16'# :: Word# -> Word# -> Int#
eqWord16'# = eqWord#
eqWord32'# :: Word# -> Word# -> Int#
eqWord32'# = eqWord#

-- No need to tick wrap these, they didn't exist <9.2
word16ToInt16# :: Word# -> Int#
word16ToInt16# w = narrow16Int# (word2Int# w)
word32ToInt32# :: Word# -> Int#
word32ToInt32# w = narrow32Int# (word2Int# w)
{-# inline word16ToInt16# #-}
{-# inline word32ToInt32# #-}

-- "Sized" wrappers: sized on all
indexWord8OffAddr''# :: Addr# -> Int# -> Word8#
indexWord8OffAddr''# a# i# = narrowWord8# (indexWord8OffAddr# a# i#)
wordToWord8''# :: Word# -> Word8#
wordToWord8''# = narrowWord8#
word8ToWord''# :: Word8# -> Word#
word8ToWord''# = extendWord8#

#endif

{-# inline byteSwap16'# #-}
{-# inline byteSwap32'# #-}
{-# inline eqWord8'#    #-}
{-# inline eqWord16'#   #-}
{-# inline eqWord32'#   #-}

{-# inline indexWord8OffAddr''#  #-}
{-# inline wordToWord8''# #-}
{-# inline word8ToWord''# #-}
