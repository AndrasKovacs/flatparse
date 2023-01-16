{-# LANGUAGE UnboxedTuples, BinaryLiterals #-}

module FlatParse.Common.Numbers where

import FlatParse.Common.Assorted ( shortInteger )

import GHC.Exts
import GHC.Word
import GHC.ForeignPtr

import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Internal as B

mul10 :: Int# -> Int#
mul10 n = uncheckedIShiftL# n 3# +# uncheckedIShiftL# n 1#
{-# inline mul10 #-}

readInt' :: Int# -> Addr# -> Addr# -> (# Int#, Addr# #)
readInt' acc s end = case eqAddr# s end of
  1# -> (# acc, s #)
  _  ->
    let w# = indexWord8OffAddr# s 0#
    in  if   0x30 <= W8# w# && W8# w# <= 0x39
        then
          let !(I# w'#) = fromIntegral (W8# w#)
          in  readInt' (mul10 acc +# (w'# -# 0x30#)) (plusAddr# s 1#) end
        else (# acc, s #)

-- | Read an `Int` from the input, as a non-empty digit sequence. The `Int` may
--   overflow in the result.
readInt :: Addr# -> Addr# -> (# (##) | (# Int#, Addr# #) #)
readInt eob s = case readInt' 0# s eob of
  (# n, s' #) | 1# <- eqAddr# s s' -> (# (##) | #)
              | otherwise          -> (# | (# n, s' #) #)
{-# inline readInt #-}

-- | Read an `Integer` from the input, as a non-empty digit sequence.
readInteger :: ForeignPtrContents -> Addr# -> Addr# -> (# (##) | (# Integer, Addr# #) #)
readInteger fp eob s = case readInt' 0# s eob of
  (# n, s' #)
    | 1# <- eqAddr# s s'            -> (# (##) | #)
    | 1# <- minusAddr# s' s <=# 18# -> (# | (# shortInteger n, s' #) #)
    | otherwise -> case BC8.readInteger (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) of
        Nothing     -> (# (##) | #)
        Just (i, _) -> (# | (# i, s' #) #)
{-# inline readInteger #-}

readIntHex' :: Int# -> Addr# -> Addr# -> (# Int#, Addr# #)
readIntHex' acc s end = case eqAddr# s end of
  1# -> (# acc, s #)
  _  ->
    let w# = indexWord8OffAddr# s 0#
    in  if 0x30 <= W8# w# && W8# w# <= 0x39
        then let !(I# w'#) = fromIntegral (W8# w#)
             in  readIntHex' (uncheckedIShiftL# acc 4# +# (w'# -# 0x30#)) (plusAddr# s 1#) end
        else if 0x41 <= W8# w# && W8# w# <= 0x46
        then let !(I# w'#) = fromIntegral (W8# w#)
             in  readIntHex' (uncheckedIShiftL# acc 4# +# (w'# -# 0x37#)) (plusAddr# s 1#) end
        else if 0x61 <= W8# w# && W8# w# <= 0x66
        then let !(I# w'#) = fromIntegral (W8# w#)
             in  readIntHex' (uncheckedIShiftL# acc 4# +# (w'# -# 0x57#)) (plusAddr# s 1#) end
        else (# acc, s #)

-- | Read an `Int` from the input, as a non-empty case-insensitive ASCII
--   hexadecimal digit sequence. The `Int` may overflow in the result.
{-# INLINE readIntHex #-}
readIntHex :: Addr# -> Addr# -> (# (##) | (# Int#, Addr# #) #)
readIntHex eob s = case readIntHex' 0# s eob of
    (# n, s' #) | 1# <- eqAddr# s s' -> (# (##) | #)
                | otherwise          -> (# | (# n, s' #) #)

-- | protobuf style (LE, redundant, on continues)
getVarintProtobuf# :: Addr# -> Addr# -> (# (##) | (# Int#, Addr#, Int# #) #)
-- v TODO not working??
#if !MIN_VERSION_base(4,17,0)
getVarintProtobuf# = error "TODO not supported on this GHC bud"
#else
getVarintProtobuf# end# = go 0# 0#
  where
    go :: Int# -> Int# -> Addr# -> (# (##) | (# Int#, Addr#, Int# #) #)
    go i# n# s# = case eqAddr# s# end# of
      1# -> (# (##) | #)
      _  ->
        let w# = indexWord8OffAddr# s# 0#
            w'# = word2Int# (word8ToWord# (w# `andWord8#` (wordToWord8# 0b01111111##)))
            i'# = i# `orI#` (w'# `uncheckedIShiftL#` n#)
        in  case w# `geWord8#` wordToWord8# 0b10000000## of
              1# -> go i'# (n# +# 7#) (s# `plusAddr#` 1#)
              _  -> (# | (# i'#, s#, n# #) #)
#endif
