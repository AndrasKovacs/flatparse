{-# LANGUAGE DerivingStrategies, DerivingVia #-}

module FlatParse.Common.Position
  ( Pos(..), endPos, addrToPos#, posToAddr#
  , Span(..), unsafeSlice, leftPos, rightPos
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.Int
import GHC.ForeignPtr ( ForeignPtr(..) )
import GHC.Exts

-- | Byte offset counted backwards from the end of the buffer.
--   Note: the `Ord` instance for `Pos` considers the earlier positions to be
--   smaller.
newtype Pos = Pos { unPos :: Int }
    deriving stock (Show)
    deriving Eq via Int

instance Ord Pos where
  (<=) (Pos x) (Pos y) = y <= x
  {-# inline (<=) #-}
  compare (Pos x) (Pos y) = compare y x
  {-# inline compare #-}

-- | A pair of positions.
data Span = Span !Pos !Pos
    deriving stock (Eq, Show)

-- | Very unsafe conversion between a primitive address and a position.  The
--   first argument points to the end of the buffer, the second argument is
--   being converted.
addrToPos# :: Addr# -> Addr# -> Pos
addrToPos# eob s = Pos (I# (minusAddr# eob s))
{-# inline addrToPos# #-}

-- | Very unsafe conversion between a primitive address and a position.  The
--   first argument points to the end of the buffer.
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

-- | The end of the input.
endPos :: Pos
endPos = Pos 0
{-# inline endPos #-}

leftPos :: Span -> Pos
leftPos (Span p _) = p
{-# inline leftPos #-}

rightPos :: Span -> Pos
rightPos (Span _ p) = p
{-# inline rightPos #-}
