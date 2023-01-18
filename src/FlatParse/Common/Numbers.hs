{-# LANGUAGE UnboxedTuples, BinaryLiterals #-}

module FlatParse.Common.Numbers where

import FlatParse.Common.Assorted ( shortInteger )

import GHC.Exts
import GHC.ForeignPtr

import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Internal as B

-- | Parse a non-empty ASCII decimal digit sequence as a 'Word'.
--   Fails on overflow.
anyAsciiDecimalWord# :: Addr# -> Addr# -> (# (##) | (# Word#, Addr# #) #)
anyAsciiDecimalWord# eob s = case anyAsciiDecimalWord_# 0## eob s of
    (# | (# n, s' #) #) | 0# <- eqAddr# s s'
                        -> (# | (# n, s' #) #)
    _                   -> (# (# #) | #)
{-# inline anyAsciiDecimalWord# #-}

-- | Parse a non-empty ASCII decimal digit sequence as a positive 'Int'.
--   Fails on overflow.
anyAsciiDecimalInt# :: Addr# -> Addr# -> (# (##) | (# Int#, Addr# #) #)
anyAsciiDecimalInt# eob s = case anyAsciiDecimalWord_# 0## eob s of
    (# | (# n, s' #) #) | 0# <- eqAddr# s s'
                        , 1# <- leWord# n (int2Word# (unI# maxBound))
                        -> (# | (# word2Int# n, s' #) #)
    _                   -> (# (##) | #)
{-# inline anyAsciiDecimalInt# #-}

anyAsciiDecimalWord_# :: Word# -> Addr# -> Addr# -> (# (##) | (# Word#, Addr# #) #)
anyAsciiDecimalWord_# acc eob s = case eqAddr# s eob of
  1# -> (# | (# acc, s #) #)
  _  -> case indexWord8OffAddr# s 0# of
#if MIN_VERSION_base(4,16,0)
    w | 1# <- leWord8# (wordToWord8# 0x30##) w
      , 1# <- leWord8# w (wordToWord8# 0x39##)
      -> case timesWord2# acc 10## of
          (# 0##, r #) -> case addWordC# r (word8ToWord# w `minusWord#` 0x30##) of
#else
    w | 1# <- leWord# 0x30## w
      , 1# <- leWord# w 0x39##
      -> case timesWord2# acc 10## of
          (# 0##, r #) -> case addWordC# r (w `minusWord#` 0x30##) of
#endif
            (# q, 0# #) -> anyAsciiDecimalWord_# q eob (s `plusAddr#` 1#)
            _           -> (# (##) | #)
          _             -> (# (##) | #)
    _ -> (# | (# acc, s #) #)

--------------------------------------------------------------------------------

-- | Parse a non-empty ASCII decimal digit sequence as a positive 'Int'.
--   May overflow.
anyAsciiDecimalIntOverflow# :: Addr# -> Addr# -> (# (##) | (# Int#, Addr# #) #)
anyAsciiDecimalIntOverflow# eob s = case anyAsciiDecimalIntOverflow_# 0# eob s of
    (# n, s' #) | 0# <- eqAddr# s s'
                -> (# | (# n, s' #) #)

                | otherwise
                -> (# (##) | #)
{-# inline anyAsciiDecimalIntOverflow# #-}

-- | Parse a non-empty ASCII decimal digit sequence as a positive 'Integer'.
anyAsciiDecimalInteger# :: ForeignPtrContents -> Addr# -> Addr# -> (# (##) | (# Integer, Addr# #) #)
anyAsciiDecimalInteger# fp eob s = case anyAsciiDecimalIntOverflow_# 0# eob s of
  (# n, s' #)
    | 1# <- eqAddr# s s'            -> (# (##) | #)

    -- Simple heuristic, 18 digits correspond to somewhere between 2^59 and 2^60, which is
    -- well inside the 'IS' constructor.
    | 1# <- minusAddr# s' s <=# 18# -> (# | (# shortInteger n, s' #) #)
    | otherwise -> case BC8.readInteger (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) of
        Nothing     -> (# (##) | #)
        Just (i, _) -> (# | (# i, s' #) #)
{-# inline anyAsciiDecimalInteger# #-}

-- | Parse a non-empty ASCII decimal digit sequence as a positive 'Int'.
--   May overflow.
anyAsciiDecimalIntOverflow_# :: Int# -> Addr# -> Addr# -> (# Int#, Addr# #)
anyAsciiDecimalIntOverflow_# acc eob s = case eqAddr# s eob of
  1# -> (# acc, s #)
  _  -> case indexWord8OffAddr# s 0# of
#if MIN_VERSION_base(4,16,0)
    w | 1# <- leWord8# (wordToWord8# 0x30##) w, 1# <- leWord8# w (wordToWord8# 0x39##) ->
      anyAsciiDecimalIntOverflow_# (mul10# acc +# (word2Int# (word8ToWord# w) -# 0x30#)) eob (plusAddr# s 1#)
#else
    w | 1# <- leWord# 0x30## w, 1# <- leWord# w 0x39## ->
      anyAsciiDecimalIntOverflow_# (mul10# acc +# (word2Int# w -# 0x30#)) eob (plusAddr# s 1#)
#endif
    _ -> (# acc, s #)

--------------------------------------------------------------------------------

-- | Parse a non-empty, case-insensitive ASCII hexadecimal digit sequence as a
--   'Word'.
--   Fails on overflow.
anyAsciiHexWord# :: Addr# -> Addr# -> (# (##) | (# Word#, Addr# #) #)
anyAsciiHexWord# eob s = case anyAsciiHexWord_# 0## eob s of
    (# | (# n, s' #) #) | 0# <- eqAddr# s s'
                        -> (# | (# n, s' #) #)
    _                   -> (# (# #) | #)
{-# inline anyAsciiHexWord# #-}

-- | Parse a non-empty, case-insensitive ASCII hexadecimal digit sequence as a
--   positive 'Int'.
--   Fails on overflow.
anyAsciiHexInt# :: Addr# -> Addr# -> (# (##) | (# Int#, Addr# #) #)
anyAsciiHexInt# eob s = case anyAsciiHexWord_# 0## eob s of
    (# | (# n, s' #) #) | 0# <- eqAddr# s s'
                        , 1# <- leWord# n (int2Word# (unI# maxBound))
                        -> (# | (# word2Int# n, s' #) #)

                        | otherwise
                        -> (# (##) | #)
    (# (##) | #)        -> (# (##) | #)
{-# inline anyAsciiHexInt# #-}

anyAsciiHexWord_# :: Word# -> Addr# -> Addr# -> (# (##) | (# Word#, Addr# #) #)
anyAsciiHexWord_# acc eob s = case eqAddr# s eob of
  1# -> (# | (# acc, s #) #)
  _  -> case indexWord8OffAddr# s 0# of
#if MIN_VERSION_base(4,16,0)
    w | 1# <- leWord8# (wordToWord8# 0x30##) w
      , 1# <- leWord8# w (wordToWord8# 0x39##)
      -> case timesWord2# acc 16## of
          (# 0##, r #) -> case addWordC# r (word8ToWord# w `minusWord#` 0x30##) of
#else
    w | 1# <- leWord# 0x30## w
      , 1# <- leWord# w 0x39##
      -> case timesWord2# acc 16## of
          (# 0##, r #) -> case addWordC# r (w `minusWord#` 0x30##) of
#endif
            (# q, 0# #) -> anyAsciiHexWord_# q eob (s `plusAddr#` 1#)
            _           -> (# (##) | #)
          _             -> (# (##) | #)
#if MIN_VERSION_base(4,16,0)
      | 1# <- leWord8# (wordToWord8# 0x41##) w
      , 1# <- leWord8# w (wordToWord8# 0x46##)
      -> case timesWord2# acc 16## of
          (# 0##, r #) -> case addWordC# r (word8ToWord# w `minusWord#` 0x37##) of
#else
      | 1# <- leWord# 0x41## w
      , 1# <- leWord# w 0x46##
      -> case timesWord2# acc 16## of
          (# 0##, r #) -> case addWordC# r (w `minusWord#` 0x37##) of
#endif
            (# q, 0# #) -> anyAsciiHexWord_# q eob (s `plusAddr#` 1#)
            _           -> (# (##) | #)
          _             -> (# (##) | #)
#if MIN_VERSION_base(4,16,0)
      | 1# <- leWord8# (wordToWord8# 0x61##) w
      , 1# <- leWord8# w (wordToWord8# 0x66##)
      -> case timesWord2# acc 16## of

          (# 0##, r #) -> case addWordC# r (word8ToWord# w `minusWord#` 0x57##) of
#else
      | 1# <- leWord# 0x61## w
      , 1# <- leWord# w 0x66##
      -> case timesWord2# acc 16## of

          (# 0##, r #) -> case addWordC# r (w `minusWord#` 0x57##) of
#endif
            (# q, 0# #) -> anyAsciiHexWord_# q eob (s `plusAddr#` 1#)
            _           -> (# (##) | #)
          _             -> (# (##) | #)
    _ -> (# | (# acc, s #) #)

--------------------------------------------------------------------------------
-- Zigzag encoding
-- See: https://hackage.haskell.org/package/zigzag-0.0.1.0/docs/src/Data.Word.Zigzag.html

fromZigzagNative :: Word -> Int
fromZigzagNative (W# w#) = I# (fromZigzagNative# w#)
{-# inline fromZigzagNative #-}

-- GHC should optimize to this, but to be sure, here it is
fromZigzagNative# :: Word# -> Int#
fromZigzagNative# w# =
    word2Int# ((w# `uncheckedShiftRL#` 1#) `xor#` (not# (w# `and#` 1##) `plusWord#` 1##))
{-# inline fromZigzagNative# #-}

toZigzagNative :: Int -> Word
toZigzagNative (I# i#) = W# (toZigzagNative# i#)
{-# inline toZigzagNative #-}

-- GHC should optimize to this, but to be sure, here it is
toZigzagNative# :: Int# -> Word#
toZigzagNative# i# = toZigzagNative'# (int2Word# i#)
{-# inline toZigzagNative# #-}

-- GHC should optimize to this, but to be sure, here it is
toZigzagNative'# :: Word# -> Word#
toZigzagNative'# w# =
    (w# `uncheckedShiftL#` 1#) `xor#` (w# `uncheckedShiftRL#` 63#)
{-# inline toZigzagNative'# #-}

--------------------------------------------------------------------------------

-- | protobuf style (LE, redundant, on continues)
anyVarintProtobuf# :: Addr# -> Addr# -> (# (##) | (# Int#, Addr#, Int# #) #)

#if MIN_VERSION_base(4,16,0)

anyVarintProtobuf# end# = go 0# 0#
  where
    word8ToInt# :: Word8# -> Int#
    word8ToInt# w8# = word2Int# (word8ToWord# w8#)
    {-# inline word8ToInt# #-}
    go :: Int# -> Int# -> Addr# -> (# (##) | (# Int#, Addr#, Int# #) #)
    go i# n# s# = case eqAddr# s# end# of
      1# -> (# (##) | #)
      _  ->
        let w8# = indexWord8OffAddr# s# 0#
            w8'# = word8ToInt# (w8# `andWord8#` (wordToWord8# 0b01111111##))
            i'# = i# `orI#` (w8'# `uncheckedIShiftL#` n#)
            s'# = s# `plusAddr#` 1#
            n'# = n# +# 7#
        in  case w8# `geWord8#` wordToWord8# 0b10000000## of
              1# -> go i'# n'# s'#
              _  -> (# | (# i'#, s'#, n'# #) #)

#else

anyVarintProtobuf# end# = go 0# 0#
  where
    go :: Int# -> Int# -> Addr# -> (# (##) | (# Int#, Addr#, Int# #) #)
    go i# n# s# = case eqAddr# s# end# of
      1# -> (# (##) | #)
      _  ->
        let w8# = indexWord8OffAddr# s# 0#
            w8'# = word2Int# (w8# `and#` 0b01111111##)
            i'# = i# `orI#` (w8'# `uncheckedIShiftL#` n#)
            s'# = s# `plusAddr#` 1#
            n'# = n# +# 7#
        in  case w8# `geWord#` 0b10000000## of
              1# -> go i'# n'# s'#
              _  -> (# | (# i'#, s'#, n'# #) #)

#endif

--------------------------------------------------------------------------------

unI# :: Int -> Int#
unI# (I# i) = i
{-# inline unI# #-}

mul10# :: Int# -> Int#
mul10# n = uncheckedIShiftL# n 3# +# uncheckedIShiftL# n 1#
{-# inline mul10# #-}
