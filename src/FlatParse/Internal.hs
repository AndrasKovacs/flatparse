{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module FlatParse.Internal where

import FlatParse.Internal.UnboxedNumerics


import Data.Bits
import Data.Char
import Data.Foldable (foldl')
import Data.Map (Map)
import GHC.Exts
import GHC.ForeignPtr

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Internal as B
import qualified Data.Map.Strict as M

#if MIN_VERSION_base(4,15,0)
import GHC.Num.Integer (Integer(..))
#else
import GHC.Integer.GMP.Internals (Integer(..))
#endif

-- Compatibility
--------------------------------------------------------------------------------

shortInteger :: Int# -> Integer
#if MIN_VERSION_base(4,15,0)
shortInteger = IS
#else
shortInteger = S#
#endif
{-# inline shortInteger #-}


-- Char predicates
--------------------------------------------------------------------------------

-- | @isDigit c = \'0\' <= c && c <= \'9\'@
isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'
{-# inline isDigit #-}

-- | @isLatinLetter c = (\'A\' <= c && c <= \'Z\') || (\'a\' <= c && c <= \'z\')@
isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
{-# inline isLatinLetter #-}

-- | @isGreekLetter c = (\'Α\' <= c && c <= \'Ω\') || (\'α\' <= c && c <= \'ω\')@
isGreekLetter :: Char -> Bool
isGreekLetter c = ('Α' <= c && c <= 'Ω') || ('α' <= c && c <= 'ω')
{-# inline isGreekLetter #-}

-- Int(eger) reading
--------------------------------------------------------------------------------

mul10 :: Int# -> Int#
mul10 n = uncheckedIShiftL# n 3# +# uncheckedIShiftL# n 1#
{-# inline mul10 #-}

readWordHex_ :: Word# -> Addr# -> Addr# -> (# (##) | (# Word#, Addr# #) #)
readWordHex_ acc eob s = case eqAddr# s eob of
  1# -> (# | (# acc, s #) #)
  _  -> case indexWord8OffAddr''# s 0# of
    w | 1# <- leWord8# (wordToWord8''# 0x30##) w
      , 1# <- leWord8# w (wordToWord8''# 0x39##)
      -> case timesWord2# acc 16## of
          (# 0##, r #) -> case addWordC# r (word8ToWord''# w `minusWord#` 0x30##) of
            (# q, 0# #) -> readWordHex_ q eob (s `plusAddr#` 1#)
            _           -> (# (##) | #)
          _             -> (# (##) | #)
      | 1# <- leWord8# (wordToWord8''# 0x41##) w
      , 1# <- leWord8# w (wordToWord8''# 0x46##)
      -> case timesWord2# acc 16## of
          (# 0##, r #) -> case addWordC# r (word8ToWord''# w `minusWord#` 0x37##) of
            (# q, 0# #) -> readWordHex_ q eob (s `plusAddr#` 1#)
            _           -> (# (##) | #)
          _             -> (# (##) | #)
      | 1# <- leWord8# (wordToWord8''# 0x61##) w
      , 1# <- leWord8# w (wordToWord8''# 0x66##)
      -> case timesWord2# acc 16## of

          (# 0##, r #) -> case addWordC# r (word8ToWord''# w `minusWord#` 0x57##) of
            (# q, 0# #) -> readWordHex_ q eob (s `plusAddr#` 1#)
            _           -> (# (##) | #)
          _             -> (# (##) | #)
    _ -> (# | (# acc, s #) #)

-- | Read a `Word` from the input, as a non-empty ASCII encoded hexadecimal digit sequence. Fails on overflow
readWordHex :: Addr# -> Addr# -> (# (##) | (# Word#, Addr# #) #)
readWordHex eob s = case readWordHex_ 0## eob s of
    (# | (# n, s' #) #) | 0# <- eqAddr# s s'
                        -> (# | (# n, s' #) #)
    _                   -> (# (# #) | #)
{-# inline readWordHex #-}

-- | Read an `Int` from the input, as a non-empty ASCII encoded hexadecimal digit sequence. Fails on overflow.
readIntHex :: Addr# -> Addr# -> (# (##) | (# Int#, Addr# #) #)
readIntHex eob s = case readWordHex_ 0## eob s of
    (# | (# n, s' #) #) | 0# <- eqAddr# s s'
                        , 1# <- leWord# n (int2Word# (unI maxBound))
                        -> (# | (# word2Int# n, s' #) #)

                        | otherwise
                        -> (# (##) | #)
    (# (##) | #)        -> (# (##) | #)

readWord_ :: Word# -> Addr# -> Addr# -> (# (##) | (# Word#, Addr# #) #)
readWord_ acc eob s = case eqAddr# s eob of
  1# -> (# | (# acc, s #) #)
  _  -> case indexWord8OffAddr''# s 0# of
    w | 1# <- leWord8# (wordToWord8''# 0x30##) w
      , 1# <- leWord8# w (wordToWord8''# 0x39##)
      -> case timesWord2# acc 10## of
          (# 0##, r #) -> case addWordC# r (word8ToWord''# w `minusWord#` 0x30##) of
            (# q, 0# #) -> readWord_ q eob (s `plusAddr#` 1#)
            _           -> (# (##) | #)
          _             -> (# (##) | #)
    _ -> (# | (# acc, s #) #)

-- | Read an `Int` from the input, as a non-empty digit sequence. Fails on overflow.
readWord :: Addr# -> Addr# -> (# (##) | (# Word#, Addr# #) #)
readWord eob s = case readWord_ 0## eob s of
    (# | (# n, s' #) #) | 0# <- eqAddr# s s'
                        -> (# | (# n, s' #) #)
    _                   -> (# (# #) | #)
{-# inline readWord #-}


-- | Read an `Int` from the input, as a non-empty digit sequence. Fails on overflow.
readInt :: Addr# -> Addr# -> (# (##) | (# Int#, Addr# #) #)
readInt eob s = case readWord_ 0## eob s of
    (# | (# n, s' #) #) | 0# <- eqAddr# s s'
                        , 1# <- leWord# n (int2Word# (unI maxBound))
                        -> (# | (# word2Int# n, s' #) #)
    _                   -> (# (##) | #)
{-# inline readInt #-}

unI :: Int -> Int#
unI (I# i) = i
{-# inline unI #-}

-- | Read a `Word` from the input, as a non-empty ASCII encoded hexadecimal digit sequence. May overflow.
readIntOverflow_ :: Int# -> Addr# -> Addr# -> (# Int#, Addr# #)
readIntOverflow_ acc eob s = case eqAddr# s eob of
  1# -> (# acc, s #)
  _  -> case indexWord8OffAddr''# s 0# of
    w | 1# <- leWord8# (wordToWord8''# 0x30##) w, 1# <- leWord8# w (wordToWord8''# 0x39##) ->
      readIntOverflow_ (mul10 acc +# (word2Int# (word8ToWord''# w) -# 0x30#)) eob (plusAddr# s 1#)
    _ -> (# acc, s #)

-- | Read an `Int` from the input, as a non-empty digit sequence. May overflow.
readIntOverflow :: Addr# -> Addr# -> (# (##) | (# Int#, Addr# #) #)
readIntOverflow eob s = case readIntOverflow_ 0# eob s of
    (# n, s' #) | 0# <- eqAddr# s s'
                -> (# | (# n, s' #) #)

                | otherwise
                -> (# (##) | #)
{-# inline readIntOverflow #-}

-- | Read an `Integer` from the input, as a non-empty digit sequence.
readInteger :: ForeignPtrContents -> Addr# -> Addr# -> (# (##) | (# Integer, Addr# #) #)
readInteger fp eob s = case readIntOverflow_ 0# eob s of
  (# n, s' #)
    | 1# <- eqAddr# s s'            -> (# (##) | #)

    -- Simple heuristic, 18 digits correspond to somewhere between 2^59 and 2^60, which is
    -- well inside the 'IS' constructor.
    | 1# <- minusAddr# s' s <=# 18# -> (# | (# shortInteger n, s' #) #)
    | otherwise -> case BC8.readInteger (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) of
        Nothing     -> (# (##) | #)
        Just (i, _) -> (# | (# i, s' #) #)
{-# inline readInteger #-}

-- | protobuf style (LE, redundant, on continues)
--
-- also returns a count of the maximum data bits read -- if this is >64, the
-- result is overflowed and useless. we could also do this using the 'Addr#'
-- returned like the other @readX@ functions. But we're forced to keep track of
-- this number in the algo, so I guess we might as well?
readVarintProtobuf# :: Addr# -> Addr# -> (# (##) | (# Word#, Addr#, Int# #) #)
readVarintProtobuf# end# = go 0## 0#
  where
    go :: Word# -> Int# -> Addr# -> (# (##) | (# Word#, Addr#, Int# #) #)
    go i# n# s# = case eqAddr# s# end# of
      1# -> (# (##) | #)
      _  ->
        let w# = indexWord8OffAddr''# s# 0#
            w'# = word8ToWord''# (w# `andWord8#` (wordToWord8''# 0b01111111##))
            i'# = i# `or#` (w'# `uncheckedShiftL#` n#)
            s'# = s# `plusAddr#` 1#
            n'# = n# +# 7#
        in  case w# `geWord8#` wordToWord8''# 0b10000000## of
              1# -> go i'# n'# s'#
              _  -> (# | (# i'#, s'#, n'# #) #)

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

-- UTF conversions
--------------------------------------------------------------------------------

-- | Convert a `String` to an UTF-8-coded `B.ByteString`.
packUTF8 :: String -> B.ByteString
packUTF8 str = B.pack $ do
  c <- str
  w <- charToBytes c
  pure (fromIntegral w)

charToBytes :: Char -> [Word]
charToBytes c'
    | c <= 0x7f     = [fromIntegral c]
    | c <= 0x7ff    = [0xc0 .|. y, 0x80 .|. z]
    | c <= 0xffff   = [0xe0 .|. x, 0x80 .|. y, 0x80 .|. z]
    | c <= 0x10ffff = [0xf0 .|. w, 0x80 .|. x, 0x80 .|. y, 0x80 .|. z]
    | otherwise = error "Not a valid Unicode code point"
  where
    c = ord c'
    z = fromIntegral (c                 .&. 0x3f)
    y = fromIntegral (unsafeShiftR c 6  .&. 0x3f)
    x = fromIntegral (unsafeShiftR c 12 .&. 0x3f)
    w = fromIntegral (unsafeShiftR c 18 .&. 0x7)

strToBytes :: String -> [Word]
strToBytes = concatMap charToBytes
{-# inline strToBytes #-}

packBytes :: [Word] -> Word
packBytes = fst . foldl' go (0, 0) where
  go (acc, shift) w | shift == 64 = error "packWords: too many bytes"
  go (acc, shift) w = (unsafeShiftL (fromIntegral w) shift .|. acc, shift+8)

splitBytes :: [Word] -> ([Word], [Word])
splitBytes ws = case quotRem (length ws) 8 of
  (0, _) -> (ws, [])
  (_, r) -> (as, chunk8s bs) where
              (as, bs) = splitAt r ws
              chunk8s [] = []
              chunk8s ws = let (as, bs) = splitAt 8 ws in
                           packBytes as : chunk8s bs

-- Switch trie compilation
--------------------------------------------------------------------------------

data Trie a = Branch !a !(Map Word (Trie a))
  deriving Show

type Rule = Maybe Int

nilTrie :: Trie Rule
nilTrie = Branch Nothing mempty

updRule :: Int -> Maybe Int -> Maybe Int
updRule rule = Just . maybe rule (min rule)

insert :: Int -> [Word] -> Trie Rule -> Trie Rule
insert rule = go where
  go [] (Branch rule' ts) =
    Branch (updRule rule rule') ts
  go (c:cs) (Branch rule' ts) =
    Branch rule' (M.alter (Just . maybe (go cs nilTrie) (go cs)) c ts)

listToTrie :: [(Int, String)] -> Trie Rule
listToTrie = foldl' (\t (!r, !s) -> insert r (charToBytes =<< s) t) nilTrie

-- | Decorate a trie with the minimum lengths of non-empty paths. This
--   is used later to place `ensureBytes#`.
mindepths :: Trie Rule -> Trie (Rule, Int)
mindepths (Branch rule ts) =
  if M.null ts then
    Branch (rule, 0) mempty
  else
    let !ts' = M.map mindepths ts in
    Branch (
      rule,
      minimum (M.map (\(Branch (rule,d) _) -> maybe (d + 1) (\_ -> 1) rule) ts'))
      ts'

data Trie' a
  = Branch' !a !(Map Word (Trie' a))
  | Path !a ![Word] !(Trie' a)
  deriving Show

-- | Compress linear paths.
pathify :: Trie (Rule, Int) -> Trie' (Rule, Int)
pathify (Branch a ts) = case M.toList ts of
  [] -> Branch' a mempty
  [(w, t)] -> case pathify t of
           Path (Nothing, _) ws t -> Path a (w:ws) t
           t                      -> Path a [w] t
  _   -> Branch' a (M.map pathify ts)

-- | Compute where to fall back after we exhausted a branch. If the branch is
--   empty, that means we've succeded at reading and we jump to the rhs rule.
fallbacks :: Trie' (Rule, Int) -> Trie' (Rule, Int, Int)
fallbacks = go Nothing 0  where
  go :: Rule -> Int -> Trie' (Rule, Int) -> Trie' (Rule, Int, Int)
  go !rule !n (Branch' (rule', d) ts)
    | M.null ts        = Branch' (rule', 0, d) mempty
    | Nothing <- rule' = Branch' (rule, n, d) (go rule (n + 1) <$> ts)
    | otherwise        = Branch' (rule', 0, d) (go rule' 1     <$> ts)
  go rule n (Path (rule', d) ws t)
    | Nothing <- rule' = Path (rule, n, d)  ws (go rule (n + length ws) t)
    | otherwise        = Path (rule', 0, d) ws (go rule' (length ws) t)

-- | Decorate with `ensureBytes#` invocations, represented as
--   `Maybe Int`.
ensureBytes :: Trie' (Rule, Int, Int) -> Trie' (Rule, Int, Maybe Int)
ensureBytes = go 0 where
  go :: Int -> Trie' (Rule, Int, Int) -> Trie' (Rule, Int, Maybe Int)
  go !res = \case
    Branch' (r, n, d) ts
      | M.null ts -> Branch' (r, n, Nothing) mempty
      | res < 1   -> Branch' (r, n, Just d ) (go (d   - 1) <$> ts)
      | otherwise -> Branch' (r, n, Nothing) (go (res - 1) <$> ts)
    Path (r, n, d) ws t -> case length ws of
      l | res < l   -> Path (r, n, Just $! d - res) ws (go (d - l)   t)
        | otherwise -> Path (r, n, Nothing        ) ws (go (res - l) t)

compileTrie :: [(Int, String)] -> Trie' (Rule, Int, Maybe Int)
compileTrie = ensureBytes . fallbacks . pathify . mindepths . listToTrie

-- These type aliases are used as parameters to ParserT
{-
data Pure
type IOMode = State# RealWorld
type PureMode = Proxy# Pure
type STMode s = State# s

#if !MIN_VERSION_base(4,17,0)
type ZeroBitRep = 'TupleRep ('[] :: [RuntimeRep])
type ZeroBitType = TYPE ZeroBitRep
#endif
-}
