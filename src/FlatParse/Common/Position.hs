module FlatParse.Common.Position where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.Int
import GHC.ForeignPtr ( ForeignPtr(..) )
import GHC.Exts

-- | Byte offset counted backwards from the end of the buffer.
--
-- TODO can't use unlifted 'Int#' because of type classes. bugger! some day
newtype Pos = Pos Int deriving (Eq, Show)

-- | A pair of positions.
data Span = Span !Pos !Pos deriving (Eq, Show)

instance Ord Pos where
  Pos p <= Pos p' = p' <= p
  Pos p <  Pos p' = p' <  p
  Pos p >  Pos p' = p' >  p
  Pos p >= Pos p' = p' >= p
  {-# inline (<=) #-}
  {-# inline (<) #-}
  {-# inline (>) #-}
  {-# inline (>=) #-}

addrToPos# :: Addr# -> Addr# -> Pos
addrToPos# eob s = Pos (I# (minusAddr# eob s))
{-# inline addrToPos# #-}

posToAddr# :: Addr# -> Pos -> Addr#
posToAddr# eob (Pos (I# n)) = unsafeCoerce# (minusAddr# eob (unsafeCoerce# n))
{-# inline posToAddr# #-}

-- | Slice into a `B.ByteString` using a `Span`. The result is invalid if the `Span`
--   is not a valid slice of the first argument.
unsafeSlice :: B.ByteString -> Span -> B.ByteString
unsafeSlice (B.PS (ForeignPtr addr fp) (I# start) (I# len))
            (Span (Pos (I# o1)) (Pos (I# o2))) =
  let end = addr `plusAddr#` start `plusAddr#` len
  in B.PS (ForeignPtr (plusAddr# end (negateInt# o1)) fp) (I# 0#) (I# (o1 -# o2))
{-# inline unsafeSlice #-}
