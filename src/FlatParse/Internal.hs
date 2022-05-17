{-# language UnboxedTuples #-}

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
import GHC.Num.Natural (Natural(..), naturalFromBigNat# )
#else
import GHC.Integer.GMP.Internals (Integer(..))
import GHC.Natural (Natural(..), naturalFromInteger, wordToNatural# )
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

mul10 :: Word# -> Word#
mul10 n = uncheckedShiftL# n 3# `plusWord#` uncheckedShiftL# n 1#
{-# inline mul10 #-}

readAsciiWord' :: Word# -> Addr# -> Addr# -> (# Word#, Addr# #)
readAsciiWord' acc s end = case eqAddr# s end of
  1# -> (# acc, s #)
  _  -> case indexWord8OffAddr''# s 0# of
    w | 1# <- leWord8# (wordToWord8''# 0x30##) w, 1# <- leWord8# w (wordToWord8''# 0x39##) ->
      readAsciiWord' (mul10 acc `plusWord#` (word8ToWord''# w `minusWord#` 0x30##)) (plusAddr# s 1#) end
    _ -> (# acc, s #)

-- | Read a non-empty ASCII digit sequence into a 'Word'. Will overflow on
--   inputs over @maxBound Word@.
readAsciiWord :: Addr# -> Addr# -> (# (##) | (# Word#, Addr# #) #)
readAsciiWord eob s = case readAsciiWord' 0## s eob of
  (# n, s' #) | 1# <- eqAddr# s s' -> (# (##) | #)
              | otherwise          -> (# | (# n, s' #) #)
{-# inline readAsciiWord #-}

readAsciiNatural :: ForeignPtrContents -> Addr# -> Addr# -> (# (##) | (# Natural, Addr# #) #)
readAsciiNatural fp eob s = case readAsciiWord' 0## s eob of
  (# n#, s' #)
    -- no digits read
    | 1# <- eqAddr# s s'            -> (# (##) | #)

    -- if number is 19 digits long or fewer, no overflow could have occurred,
    -- because @maxBound :: Word@ has 20 digits
    | 1# <- minusAddr# s' s <=# 19# ->
#if MIN_VERSION_base(4,15,0)
        (# | (# NS n#, s' #) #)
#else
        (# | (# wordToNatural# n#, s' #) #)
#endif

    -- else there might be an overflow: reparse with slower code from bytestring
    -- the output can only be positive because we don't parse sign prefixes here
    -- (so it would've failed with no digits read)
    | otherwise -> case BC8.readInteger (B.PS (ForeignPtr s fp) 0 (I# (minusAddr# s' s))) of
        Nothing     -> (# (##) | #)

#if MIN_VERSION_base(4,15,0)
        -- 'Integer''s bignat doesn't go straight into 'Natural' due to
        -- invariant differences, so need to use a helper
        Just ((IP n#), _) -> (# | (# naturalFromBigNat# n#, s' #) #)

        -- this actually can't occur because invariant for 'IS' means it has to
        -- be 19 digits or less, but that case is covered above
        Just ((IS i#), _) -> error "impossible" -- (# | (# NS (int2Word# i#), s' #) #)

        -- negative result: just impossible
        Just ((IN i#), _) -> error "impossible"
#else
        -- urgh, don't care about pre-9.2. use slow safe function
        Just (i, _) -> (# | (# naturalFromInteger i , s' #) #)
#endif
{-# inline readAsciiNatural #-}

-- Positions and spans
--------------------------------------------------------------------------------

-- | Byte offset counted backwards from the end of the buffer.
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

derefChar8# :: Addr# -> Char#
derefChar8# addr = indexCharOffAddr# addr 0#
{-# inline derefChar8# #-}

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
