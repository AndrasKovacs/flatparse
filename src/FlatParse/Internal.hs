
module FlatParse.Internal (readInt, readInteger) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B

import GHC.Exts
import GHC.ForeignPtr
import GHC.Num.Integer (Integer(..))

mul10 :: Int# -> Int#
mul10 n = uncheckedIShiftL# n 3# +# uncheckedIShiftL# n 1#
{-# inline mul10 #-}

readInt' :: Int# -> Addr# -> Addr# -> (# Int#, Addr# #)
readInt' acc s end = case eqAddr# s end of
  1# -> (# acc, s #)
  _  -> case indexWord8OffAddr# s 0# of
    w | 1# <- leWord# 48## w, 1# <- leWord# w 57## ->
      readInt' (mul10 acc +# (word2Int# w -# 48#)) (plusAddr# s 1#) end
    _ -> (# acc, s #)


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
    | 1# <- minusAddr# s' s <=# 18# -> (# | (# IS n, s' #) #)
    | otherwise -> case B.readInteger (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) of
        Nothing     -> (# (##) | #)
        Just (i, _) -> (# | (# i, s' #) #)
{-# inline readInteger #-}
