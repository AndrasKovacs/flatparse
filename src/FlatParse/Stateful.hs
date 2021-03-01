
module FlatParse.Stateful (
    type Parser(..)
  , pattern OK#
  , pattern Fail#
  , pattern Err#

  , (<|>)
  , linesUTF8
  , Pos(..)
  , Result(..)
  , Span(..)
  , addr2Pos#
  , anyChar
  , anyCharA
  , anyCharA_
  , anyChar_
  , ask
  , br
  , chainl
  , chainr
  , lookahead
  , fails
  , char
  , cut
  , empty
  , ensureBytes#
  , eof
  , err
  , get
  , getPos
  , inSpan
  , isDigit
  , isGreekLetter
  , isLatinLetter
  , local
  , many
  , many_
  , mkPos
  , modify
  , optional
  , optioned
  , packUTF8
  , pos2Addr#
  , posLineCol
  , put
  , runParser
  , satisfy
  , satisfyA
  , satisfyA_
  , satisfy'
  , scan16#
  , scan32#
  , scan64#
  , scan8#
  , scanAny8#
  , scanBytes#
  , scanPartial64#
  , setBack#
  , setPos
  , some
  , some_
  , spanOf
  , spanned
  , span2ByteString
  , unsafeSpan2ByteString
  , string
  , switch
  , switch'
  , takeLine
  , takeString
  , testParser
  , testStrParser
  , traceRest
  , try
  , validPos
  ) where

import Control.Monad
import Data.Bits
import Data.Char (ord)
import Data.Foldable
import Data.Map (Map)
import Data.Word
import GHC.Exts
import GHC.Word
import Language.Haskell.TH
import System.IO.Unsafe

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Map.Strict as M


--------------------------------------------------------------------------------

type Res# e a =
  (#
    (# a, Addr#, Int# #)
  | (# #)
  | (# e #)
  #)

pattern OK# :: a -> Addr# -> Int# -> Res# e a
pattern OK# a s n = (# (# a, s, n #) | | #)

pattern Err# :: e -> Res# e a
pattern Err# e = (# | | (# e #) #)

pattern Fail# :: Res# e a
pattern Fail# = (# | (# #) | #)
{-# complete OK#, Err#, Fail# #-}

newtype Parser r e a = Parser {runParser# :: r -> Addr# -> Addr# -> Int# -> Res# e a}

instance Functor (Parser r e) where
  fmap f (Parser g) = Parser \r eob s n -> case g r eob s n of
    OK# a s n -> let !b = f a in OK# b s n
    x         -> unsafeCoerce# x
  {-# inline fmap #-}

  (<$) a' (Parser g) = Parser \r eob s n -> case g r eob s n of
    OK# a s n -> OK# a' s n
    x         -> unsafeCoerce# x
  {-# inline (<$) #-}

err :: e -> Parser r e a
err e = Parser \r eob s n -> Err# e
{-# inline err #-}

empty :: Parser r e a
empty = Parser \r eob s n -> Fail#
{-# inline empty #-}

get :: Parser r e Int
get = Parser \r eob s n -> OK# (I# n) s n
{-# inline get #-}

put :: Int -> Parser r e ()
put (I# n) = Parser \r eob s _ -> OK# () s n
{-# inline put #-}

modify :: (Int -> Int) -> Parser r e ()
modify f = Parser \r eob s n ->
  case f (I# n) of
    I# n -> OK# () s n
{-# inline modify #-}

ask :: Parser r e r
ask = Parser \r eob s n -> OK# r s n
{-# inline ask #-}

local :: (r' -> r) -> Parser r e a -> Parser r' e a
local f (Parser g) = Parser \r eob s n -> let!r' = f r in g r' eob s n
{-# inline local #-}

instance Applicative (Parser r e) where
  pure a = Parser \r eob s n -> OK# a s n
  {-# inline pure #-}
  Parser ff <*> Parser fa = Parser \r eob s n -> case ff r eob s n of
    OK# f s n -> case fa r eob s n of
      OK# a s n  -> let !b = f a in OK# b s n
      x          -> unsafeCoerce# x
    x -> unsafeCoerce# x
  {-# inline (<*>) #-}
  Parser fa <* Parser fb = Parser \r eob s n -> case fa r eob s n of
    OK# a s n   -> case fb r eob s n of
      OK# b s n -> OK# a s n
      x -> unsafeCoerce# x
    x -> unsafeCoerce# x
  {-# inline (<*) #-}
  Parser fa *> Parser fb = Parser \r eob s n -> case fa r eob s n of
    OK# a s n -> fb r eob s n
    x         -> unsafeCoerce# x
  {-# inline (*>) #-}

instance Monad (Parser r e) where
  return = pure
  {-# inline return #-}
  Parser fa >>= f = Parser \r eob s n -> case fa r eob s n of
    OK# a s n -> runParser# (f a) r eob s n
    x         -> unsafeCoerce# x
  {-# inline (>>=) #-}
  Parser fa >> Parser fb = Parser \r eob s n -> case fa r eob s n of
    OK# a s n -> fb r eob s n
    x         -> unsafeCoerce# x
  {-# inline (>>) #-}

derefChar8# :: Addr# -> Char#
derefChar8# addr = indexCharOffAddr# addr 0#
{-# inline derefChar8# #-}

eof :: Parser r e ()
eof = Parser \r eob s n -> case eqAddr# eob s of
  1# -> OK# () s n
  _  -> Fail#
{-# inline eof #-}

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'
{-# inline isDigit #-}

isLatinLetter :: Char -> Bool
isLatinLetter c = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
{-# inline isLatinLetter #-}

isGreekLetter :: Char -> Bool
isGreekLetter c = ('Α' <= c && c <= 'Ω') || ('α' <= c && c <= 'ω')
{-# inline isGreekLetter #-}

-- | Choose between two parsers. If the first parser fails, we
--   try the second one, but if the first one throws an error,
--   we propagate the error.
infixr 6 <|>
(<|>) :: Parser r e a -> Parser r e a -> Parser r e a
(<|>) (Parser f) (Parser g) = Parser \r eob s n ->
  case f r eob s n of
    Fail# -> g r eob s n
    x     -> x
{-# inline (<|>) #-}

-- | Convert a parsing error into failure.
try :: Parser r e a -> Parser r e a
try (Parser f) = Parser \r eob s n -> case f r eob s n of
  Err# _ -> Fail#
  x      -> x
{-# inline try #-}

-- | Convert a parsing failure into an error
cut :: Parser r e a -> e -> Parser r e a
cut (Parser f) e = Parser \r eob s n -> case f r eob s n of
  Fail# -> Err# e
  x     -> x
{-# inline cut #-}

optional :: Parser r e a -> Parser r e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# inline optional #-}

-- | CPS'd version of `optional` to get rid of the `Maybe` allocation.
optioned :: Parser r e a -> (a -> Parser r e b) -> Parser r e b -> Parser r e b
optioned (Parser f) just (Parser nothing) = Parser \r eob s n -> case f r eob s n of
  OK# a s n -> runParser# (just a) r eob s n
  Fail#     -> nothing r eob s n
  Err# e    -> Err# e
{-# inline optioned #-}

-- | Parse any `Char` in the ASCII range.
anyCharA :: Parser r e Char
anyCharA = Parser \r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# (C# c1) (plusAddr# buf 1#) n
      _  -> Fail#
{-# inline anyCharA #-}

-- | Skip any `Char` in the ASCII range.
anyCharA_ :: Parser r e ()
anyCharA_ = () <$ anyCharA
{-# inline anyCharA_ #-}

-- | Parse any `Char`. This parser fails if the input is empty or no valid UTF-8
-- code point can be read from the input.
anyChar :: Parser r e Char
anyChar = Parser \r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# (C# c1) (plusAddr# buf 1#) n
      _  -> case eqAddr# eob (plusAddr# buf 1#) of
        1# -> Fail#
        _ -> case indexCharOffAddr# buf 1# of
          c2 -> case c1 `leChar#` '\xDF'# of
            1# ->
              let resc = ((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#) `orI#`
                          (ord# c2 -# 0x80#)
              in OK# (C# (chr# resc)) (plusAddr# buf 2#) n
            _ -> case eqAddr# eob (plusAddr# buf 2#) of
              1# -> Fail#
              _  -> case indexCharOffAddr# buf 2# of
                c3 -> case c1 `leChar#` '\xEF'# of
                  1# ->
                    let resc = ((ord# c1 -# 0xE0#) `uncheckedIShiftL#` 12#) `orI#`
                               ((ord# c2 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                (ord# c3 -# 0x80#)
                    in OK# (C# (chr# resc)) (plusAddr# buf 3#) n
                  _ -> case eqAddr# eob (plusAddr# buf 3#) of
                    1# -> Fail#
                    _  -> case indexCharOffAddr# buf 3# of
                      c4 ->
                        let resc = ((ord# c1 -# 0xF0#) `uncheckedIShiftL#` 18#) `orI#`
                                   ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 12#) `orI#`
                                   ((ord# c3 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                    (ord# c4 -# 0x80#)
                        in OK# (C# (chr# resc)) (plusAddr# buf 4#) n
{-# inline anyChar #-}

-- | Skip any `Char`.
anyChar_ :: Parser r e ()
anyChar_ = Parser \r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# () (plusAddr# buf 1#) n
      _  ->
        let buf' =
              case c1 `leChar#` '\xDF'# of
                1# -> plusAddr# buf 2#
                _  -> case c1 `leChar#` '\xEF'# of
                    1# -> plusAddr# buf 3#
                    _ ->  plusAddr# buf 4#
        in case leAddr# buf' eob of
             1# -> OK# () buf' n
             _  -> Fail#
{-# inline anyChar_ #-}

-- | Parse a `Char` for which a predicate holds.
satisfy :: (Char -> Bool) -> Parser r e Char
satisfy f = Parser \r eob s n -> case runParser# anyChar r eob s n of
  OK# c s n | f c -> OK# c s n
  _               -> Fail#
{-#  inline satisfy #-}

-- | Parse an ASCII `Char` for which a predicate holds.
satisfyA :: (Char -> Bool) -> Parser r e Char
satisfyA f = Parser \r eob s n -> case runParser# anyCharA r eob s n of
  OK# c s n | f c -> OK# c s n
  _               -> Fail#
{-#  inline satisfyA #-}

-- | Parse an ASCII `Char` for which a predicate holds.
satisfyA_ :: (Char -> Bool) -> Parser r e ()
satisfyA_ f = () <$ satisfyA f
{-# inline satisfyA_ #-}

satisfy' :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Parser r e Char
satisfy' f1 f2 f3 f4 = Parser \r eob buf n -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# | f1 (C# c1) -> OK# (C# c1) (plusAddr# buf 1#) n
         | otherwise  -> Fail#
      _  -> case eqAddr# eob (plusAddr# buf 1#) of
        1# -> Fail#
        _ -> case indexCharOffAddr# buf 1# of
          c2 -> case c1 `leChar#` '\xDF'# of
            1# ->
              let resc = C# (chr# (((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#) `orI#`
                                   (ord# c2 -# 0x80#)))
              in case f2 resc of
                   True -> OK# resc (plusAddr# buf 2#) n
                   _    -> Fail#
            _ -> case eqAddr# eob (plusAddr# buf 2#) of
              1# -> Fail#
              _  -> case indexCharOffAddr# buf 2# of
                c3 -> case c1 `leChar#` '\xEF'# of
                  1# ->
                    let resc = C# (chr# (((ord# c1 -# 0xE0#) `uncheckedIShiftL#` 12#) `orI#`
                                         ((ord# c2 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                         (ord# c3 -# 0x80#)))
                    in case f3 resc of
                         True -> OK# resc (plusAddr# buf 3#) n
                         _    -> Fail#
                  _ -> case eqAddr# eob (plusAddr# buf 3#) of
                    1# -> Fail#
                    _  -> case indexCharOffAddr# buf 3# of
                      c4 ->
                        let resc = C# (chr# (((ord# c1 -# 0xF0#) `uncheckedIShiftL#` 18#) `orI#`
                                             ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 12#) `orI#`
                                             ((ord# c3 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                              (ord# c4 -# 0x80#)))
                        in case f4 resc of
                             True -> OK# resc (plusAddr# buf 4#) n
                             _    -> Fail#
{-# inline satisfy' #-}

-- | Skip a parser zero or more times. This fails if the given parser fails with
--   having consumed input.
many_ :: Parser r e a -> Parser r e ()
many_ (Parser f) = go where
  go = Parser \r eob s n -> case f r eob s n of
    OK# a s n -> runParser# go r eob s n
    Fail#     -> OK# () s n
    Err# e    -> Err# e
{-# inline many_ #-}

-- | Branch on a parser: if the first argument fails, continue with the second,
--   else with the third.
br :: Parser r e a -> Parser r e b -> Parser r e b -> Parser r e b
br pa pt pf = Parser \r eob s n -> case runParser# pa r eob s n of
  OK# _ s n -> runParser# pt r eob s n
  Fail#     -> runParser# pf r eob s n
  Err# e    -> Err# e
{-# inline br #-}

-- | Skip a parser one or more times. This fails if the given parser fails with
--   having consumed input.
some_ :: Parser r e a -> Parser r e ()
some_ pa = pa >> many_ pa
{-# inline some_ #-}

many :: Parser r e a -> Parser r e [a]
many p = go where
  go = ((:) <$> p <*> go) <|> pure []
{-# inline many #-}

some :: Parser r e a -> Parser r e [a]
some p = (:) <$> p <*> many p
{-# inline some #-}

chainl :: (b -> a -> b) -> Parser r e b -> Parser r e a -> Parser r e b
chainl f start elem = start >>= go where
  go b = do {!a <- elem; go $! f b a} <|> pure b
{-# inline chainl #-}

chainr :: (a -> b -> b) -> Parser r e a -> Parser r e b -> Parser r e b
chainr f elem end = go where
  go = (f <$> elem <*> go) <|> end
{-# inline chainr #-}

lookahead :: Parser r e a -> Parser r e a
lookahead (Parser f) = Parser \r eob s n ->
  case f r eob s n of
    OK# a _ _ -> OK# a s n
    x         -> x
{-# inline lookahead #-}

fails :: Parser r e a -> Parser r e ()
fails (Parser f) = Parser \r eob s n ->
  case f r eob s n of
    OK# _ _ _ -> Fail#
    Fail#     -> OK# () s n
    Err# e    -> Err# e
{-# inline fails #-}

-- | Byte offset counted backwards from the end of the buffer.
newtype Pos = Pos Int deriving (Eq, Show)

instance Ord Pos where
  Pos p <= Pos p' = p' <= p
  Pos p <  Pos p' = p' <  p
  Pos p >  Pos p' = p' >  p
  Pos p >= Pos p' = p' >= p
  {-# inline (<=) #-}
  {-# inline (<) #-}
  {-# inline (>) #-}
  {-# inline (>=) #-}

getEnd :: Parser r e Pos
getEnd = Parser \r eob s n -> OK# (Pos 0) s n
{-# inline getEnd #-}

addr2Pos# :: Addr# -> Addr# -> Pos
addr2Pos# eob s = Pos (I# (minusAddr# eob s))
{-# inline addr2Pos# #-}

pos2Addr# :: Addr# -> Pos -> Addr#
pos2Addr# eob (Pos (I# s)) = unsafeCoerce# (minusAddr# eob (unsafeCoerce# s))
{-# inline pos2Addr# #-}

getPos :: Parser r e Pos
getPos = Parser \r eob s n -> OK# (addr2Pos# eob s) s n
{-# inline getPos #-}

setPos :: Pos -> Parser r e ()
setPos s = Parser \r eob _ n -> OK# () (pos2Addr# eob s) n
{-# inline setPos #-}

data Span = Span !Pos !Pos deriving (Eq, Show)

-- | Return the consumed span of a parser.
spanOf :: Parser r e a -> Parser r e Span
spanOf (Parser f) = Parser \r eob s n -> case f r eob s n of
  OK# a s' n -> OK# (Span (addr2Pos# eob s) (addr2Pos# eob s')) s' n
  x          -> unsafeCoerce# x
{-# inline spanOf #-}

-- | Bind the result together with the span of the result. CPS's version of `spanOf`
--   for better unboxing.
spanned :: Parser r e a -> (a -> Span -> Parser r e b) -> Parser r e b
spanned (Parser f) g = Parser \r eob s n -> case f r eob s n of
  OK# a s' n -> runParser# (g a (Span (addr2Pos# eob s) (addr2Pos# eob s'))) r eob s' n
  x          -> unsafeCoerce# x
{-# inline spanned #-}

-- | Run a parser in a given input span. The state of the input and the `Int`
--   state is restored after the parser finished, so `inSpan` does not consume
--   input and has no side effect.
--   Warning: this operation may crash if the given span points outside the current parsing
--   buffer. The validity of the span is not checked! It's always safe to use `inSpan` if
--   the span comes from a previous `spanned` usage.
inSpan :: Span -> Parser r e a -> Parser r e a
inSpan (Span s eob) (Parser f) = Parser \r eob' s' n' ->
  case f r (pos2Addr# eob' eob) (pos2Addr# eob' s) n' of
    OK# a _ _ -> OK# a s' n'
    x         -> unsafeCoerce# x
{-# inline inSpan #-}

-- | Take the rest of the input as a `String`.
takeString :: Parser r e String
takeString = ((:) <$> anyChar <*> takeString) <|> pure []

-- | Get the rest of the input as a `String`, but restore parsing state.
--   This can be useful for debugging.
traceRest :: Parser r e String
traceRest = do
  span <- Span <$> getPos <*> getEnd
  inSpan span takeString

data Result e a =
    OK a Int !(B.ByteString)
  | Fail
  | Err !e
  deriving Show

instance Functor (Result e) where
  fmap f (OK a n s) = let !b = f a in OK b n s
  fmap f Fail = Fail
  fmap f (Err e) = Err e
  {-# inline fmap #-}

runParser :: Parser r e a -> r -> Int -> B.ByteString -> Result e a
runParser (Parser f) r (I# n) b = unsafeDupablePerformIO do
  B.unsafeUseAsCString b \(Ptr buf) -> do
    let !(I# len) = B.length b
    let end = plusAddr# buf len
    case f r end buf n of
      Err# e    ->
        pure (Err e)
      OK# a s n -> do
        let offset = minusAddr# s buf
        pure (OK a (I# n) (B.drop (I# offset) b))
      Fail# ->
        pure Fail
{-# noinline runParser #-}

testParser :: Parser r e a -> r -> Int -> B.ByteString -> Result e a
testParser pa r n s = runParser pa r n s

packUTF8 :: String -> B.ByteString
packUTF8 = B.pack . concatMap charToBytes

testStrParser :: Parser r e a -> r -> Int -> String -> Result e a
testStrParser pa r n s = runParser pa r n (packUTF8 s)

takeLine :: Parser r e String
takeLine = (do
  c <- anyChar
  case c of
    '\n' -> pure ""
    _    -> (c:) <$> takeLine) <|> pure ""

-- | Break an UTF8-coded `ByteString` to lines. Throws an error on invalid input.
linesUTF8 :: B.ByteString -> [String]
linesUTF8 str =
  let go = ([] <$ eof) <|> ((:) <$> takeLine <*> go)
  in case runParser go () 0 str of
    OK ls _ _ -> ls
    _         -> error "linesUTF8: invalid input"

-- | Check whether a `Pos` points into a `ByteString`.
validPos :: B.ByteString -> Pos -> Bool
validPos str pos =
  let go = do
        start <- getPos
        end   <- getEnd
        pure (start <= pos && pos <= end)
  in case runParser go () 0 str of
    OK b _ _ -> b
    _        -> error "impossible"
{-# inline validPos #-}

-- | Get the corresponding line and column number for a `Pos`.
posLineCol :: B.ByteString -> Pos -> (Int, Int)
posLineCol str pos | validPos str pos =
  let go !line !col = (do
        c <- anyChar
        if c == '\n' then go (line + 1) 0
                     else go line (col + 1)) <|> pure (line, col)
      wrap = do
        start <- getPos
        inSpan (Span start pos) (go 0 0)

  in case runParser wrap () 0 str of
    OK res _ _ -> res
    _          -> error "impossible"
posLineCol _ _ = error "posInfo: invalid position"

-- | Create a `Pos` from a line and column number. Throws an error on out-of-bounds
--   line and column numbers.
mkPos :: B.ByteString -> (Int, Int) -> Pos
mkPos str (line', col') =
  let go line col | line == line' && col == col' = getPos
      go line col = (do
        c <- anyChar
        if c == '\n' then go (line + 1) 0
                     else go line (col + 1)) <|> error "mkPos: invalid position"
  in case runParser (go 0 0) () 0 str of
    OK res _ _ -> res
    _          -> error "impossible"

-- | Create a `ByteString` from a `Span`. The first argument is the buffer the
--   `Span` points into. Always returns an empty `ByteString` if the span is
--   invalid.
span2ByteString :: B.ByteString -> Span -> B.ByteString
span2ByteString bstr (Span (Pos n) (Pos n')) =
  B.take (n - n') (B.drop (B.length bstr - n) bstr)
{-# inline span2ByteString #-}

-- | Create a `ByteString` from a `Span`. The first argument is the buffer the
--   `Span` points into.
unsafeSpan2ByteString :: B.ByteString -> Span -> B.ByteString
unsafeSpan2ByteString bstr (Span (Pos n) (Pos n')) =
  B.unsafeTake (n - n') (B.unsafeDrop (B.length bstr - n) bstr)
{-# inline unsafeSpan2ByteString #-}

-- char and string
--------------------------------------------------------------------------------

charToBytes :: Char -> [Word8]
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

strToBytes :: String -> [Word8]
strToBytes = concatMap charToBytes
{-# inline strToBytes #-}

packBytes :: [Word8] -> Word
packBytes = fst . foldl' go (0, 0) where
  go (acc, shift) w | shift == 64 = error "packWords: too many bytes"
  go (acc, shift) w = (unsafeShiftL (fromIntegral w) shift .|. acc, shift+8)

splitBytes :: [Word8] -> ([Word8], [Word])
splitBytes ws = case quotRem (length ws) 8 of
  (0, _) -> (ws, [])
  (_, r) -> (as, chunk8s bs) where
              (as, bs) = splitAt r ws
              chunk8s [] = []
              chunk8s ws = let (as, bs) = splitAt 8 ws in
                           packBytes as : chunk8s bs

scanAny8# :: Parser r e Word8
scanAny8# = Parser \r eob s n -> OK# (W8# (indexWord8OffAddr# s 0#)) (plusAddr# s 1#) n
{-# inline scanAny8# #-}

scan8# :: Word -> Parser r e ()
scan8# (W# c) = Parser \r eob s n ->
  case indexWord8OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 1#) n
      _  -> Fail#
{-# inline scan8# #-}

scan16# :: Word -> Parser r e ()
scan16# (W# c) = Parser \r eob s n ->
  case indexWord16OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 2#) n
      _  -> Fail#
{-# inline scan16# #-}

scan32# :: Word -> Parser r e ()
scan32# (W# c) = Parser \r eob s n ->
  case indexWord32OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 4#) n
      _  -> Fail#
{-# inline scan32# #-}

scan64# :: Word -> Parser r e ()
scan64# (W# c) = Parser \r eob s n ->
  case indexWord64OffAddr# s 0# of
    c' -> case eqWord# c c' of
      1# -> OK# () (plusAddr# s 8#) n
      _  -> Fail#
{-# inline scan64# #-}

scanPartial64# :: Int -> Word -> Parser r e ()
scanPartial64# (I# len) (W# w) = Parser \r eob s n ->
  case indexWordOffAddr# s 0# of
    w' -> case uncheckedIShiftL# (8# -# len) 3# of
      sh -> case uncheckedShiftL# w' sh of
        w' -> case uncheckedShiftRL# w' sh of
          w' -> case eqWord# w w' of
            1# -> OK# () (plusAddr# s len) n
            _  -> Fail#
{-# inline scanPartial64# #-}

ensureBytes# :: Int -> Parser r e ()
ensureBytes# (I# len) = Parser \r eob s n ->
  case len  <=# minusAddr# eob s of
    1# -> OK# () s n
    _  -> Fail#
{-# inline ensureBytes# #-}

setBack# :: Int -> Parser r e ()
setBack# (I# i) = Parser \r eob s n ->
  OK# () (plusAddr# s (negateInt# i)) n
{-# inline setBack# #-}

scanBytes# :: [Word8] -> Q Exp
scanBytes# bytes = do
  let !(leading, w8s) = splitBytes bytes
      !scanw8s        = go w8s where
                         go (w8:[] ) = [| scan64# w8 |]
                         go (w8:w8s) = [| scan64# w8 >> $(go w8s) |]
                         go []       = [| pure () |]
  case w8s of
    [] -> go leading
          where
            go (a:b:c:d:[]) = let !w = packBytes [a, b, c, d] in [| scan32# w |]
            go (a:b:c:d:ws) = let !w = packBytes [a, b, c, d] in [| scan32# w >> $(go ws) |]
            go (a:b:[])     = let !w = packBytes [a, b]       in [| scan16# w |]
            go (a:b:ws)     = let !w = packBytes [a, b]       in [| scan16# w >> $(go ws) |]
            go (a:[])       = [| scan8# a |]
            go []           = [| pure () |]
    _  -> case leading of

      []              -> scanw8s
      [a]             -> [| scan8# a >> $scanw8s |]
      ws@[a, b]       -> let !w = packBytes ws in [| scan16# w >> $scanw8s |]
      ws@[a, b, c, d] -> let !w = packBytes ws in [| scan32# w >> $scanw8s |]
      ws              -> let !w = packBytes ws
                             !l = length ws
                         in [| scanPartial64# l w >> $scanw8s |]

string :: String -> Q Exp
string str = do
  let !bytes = strToBytes str
      !len   = length bytes
  [| ensureBytes# len >> $(scanBytes# bytes) |]

char :: Char -> Q Exp
char c = string [c]

-- Trie switching
--------------------------------------------------------------------------------

data Trie a = Branch !a !(Map Word8 (Trie a))

type Rule = Maybe Int

nilTrie :: Trie Rule
nilTrie = Branch Nothing mempty

updRule :: Int -> Maybe Int -> Maybe Int
updRule rule = Just . maybe rule (min rule)

insert :: Int -> [Word8] -> Trie Rule -> Trie Rule
insert rule = go where
  go [] (Branch rule' ts) =
    Branch (updRule rule rule') ts
  go (c:cs) (Branch rule' ts) =
    Branch rule' (M.alter (Just . maybe (go cs nilTrie) (go cs)) c ts)

fromList :: [(Int, String)] -> Trie Rule
fromList = foldl' (\t (!r, !s) -> insert r (charToBytes =<< s) t) nilTrie

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
  = Branch' !a !(Map Word8 (Trie' a))
  | Path !a ![Word8] !(Trie' a)

-- | Compress linear paths.
pathify :: Trie (Rule, Int) -> Trie' (Rule, Int)
pathify (Branch a ts) = case M.toList ts of
  [] -> Branch' a mempty
  [(w, t)] -> case pathify t of
           Path (Nothing, _) ws t -> Path a (w:ws) t
           t                      -> Path a [w] t
  _   -> Branch' a (M.map pathify ts)

fallbacks :: Trie' (Rule, Int) -> Trie' (Rule, Int, Int)
fallbacks = go Nothing 0  where
  go :: Rule -> Int -> Trie' (Rule, Int) -> Trie' (Rule, Int, Int)
  go !rule !n (Branch' (rule', d) ts)
    | M.null ts        = Branch' (rule', 0, d) mempty
    | Nothing <- rule' = Branch' (rule, n, d) (go rule (n + 1) <$> ts)
    | otherwise        = Branch' (rule, n, d) (go rule' 1      <$> ts)
  go rule n (Path (rule', d) ws t)
    | Nothing <- rule' = Path (rule, n, d)  ws (go rule (n + 1) t)
    | otherwise        = Path (rule', 0, d) ws (go rule' (length ws) t)

-- | Decorate with `ensureBytes#` invocations, represented as
--   `Maybe Int`.
ensureBytes :: Trie' (Rule, Int, Int) -> Trie' (Rule, Int, Maybe Int)
ensureBytes = go 0 where
  go :: Int -> Trie' (Rule, Int, Int) -> Trie' (Rule, Int, Maybe Int)
  go !res = \case
    Branch' (r, n, d) ts
      | M.null ts -> Branch' (r, n, Nothing) mempty
      |  res < 1  -> Branch' (r, n, Just d ) (go (d   - 1) <$> ts)
      | otherwise -> Branch' (r, n, Nothing) (go (res - 1) <$> ts)
    Path (r, n, d) ws t -> case length ws of
      l | res < l   -> Path (r, n, Just $! d - res) ws (go (d - l)   t)
        | otherwise -> Path (r, n, Nothing        ) ws (go (res - l) t)

compileTrie :: [(Int, String)] -> Trie' (Rule, Int, Maybe Int)
compileTrie = ensureBytes . fallbacks . pathify . mindepths . FlatParse.Stateful.fromList

genTrie :: (Map (Maybe Int) Exp, Trie' (Rule, Int, Maybe Int)) -> Q Exp
genTrie (rules, t) = do
  branches <- traverse (\e -> (,) <$> (newName "rule") <*> pure e) rules

  let ix m k = case M.lookup k m of
        Nothing -> error ("key not in map: " ++ show k)
        Just a  -> a

  let ensure :: Maybe Int -> Maybe (Q Exp)
      ensure = fmap (\n -> [| ensureBytes# n |])

      fallback :: Rule -> Int ->  Q Exp
      fallback rule 0 = pure $ VarE $ fst $ ix branches rule
      fallback rule n = [| setBack# n >> $(pure $ VarE $ fst $ ix branches rule) |]

  let go :: Trie' (Rule, Int, Maybe Int) -> Q Exp
      go = \case
        Branch' (r, n, alloc) ts
          | M.null ts -> pure $ VarE $ fst $ branches M.! r
          | otherwise -> do
              !next         <- (traverse . traverse) go (M.toList ts)
              !defaultCase  <- fallback r (n + 1)

              let cases = DoE $
                    [BindS (VarP (mkName "c")) (VarE 'scanAny8#),
                      NoBindS (CaseE (VarE (mkName "c"))
                         (map (\(w, t) ->
                                 Match (LitP (IntegerL (fromIntegral w)))
                                       (NormalB t)
                                       [])
                              next
                          ++ [Match WildP (NormalB defaultCase) []]))]

              case ensure alloc of
                Nothing    -> pure cases
                Just alloc -> [| br $alloc $(pure cases) $(fallback r n) |]

        Path (r, n, alloc) ws t ->
          case ensure alloc of
            Nothing    -> [| br $(scanBytes# ws) $(go t) $(fallback r n)|]
            Just alloc -> [| br ($alloc >> $(scanBytes# ws)) $(go t) $(fallback r n) |]

  letE
    (map (\(x, rhs) -> valD (varP x) (normalB (pure rhs)) []) (Data.Foldable.toList branches))
    (go t)

parseSwitch :: Q Exp -> Q ([(String, Exp)], Maybe Exp)
parseSwitch exp = exp >>= \case
  CaseE (UnboundVarE _) []    -> error "switch: empty clause list"
  CaseE (UnboundVarE _) cases -> do
    (!cases, !last) <- pure (init cases, last cases)
    !cases <- forM cases \case
      Match (LitP (StringL str)) (NormalB rhs) [] -> pure (str, rhs)
      _ -> error "switch: expected a match clause on a string literal"
    (!cases, !last) <- case last of
      Match (LitP (StringL str)) (NormalB rhs) [] -> pure (cases ++ [(str, rhs)], Nothing)
      Match WildP                (NormalB rhs) [] -> pure (cases, Just rhs)
      _ -> error "switch: expected a match clause on a string literal or a wildcard"
    pure (cases, last)
  _ -> error "switch: expected a \"case _ of\" expression"

genSwitchTrie' :: Maybe Exp -> [(String, Exp)] -> Maybe Exp
              -> (Map (Maybe Int) Exp, Trie' (Rule, Int, Maybe Int))
genSwitchTrie' postAction cases fallback =

  let (!branches, !strings) = unzip do
        (!i, (!str, !rhs)) <- zip [0..] cases
        case postAction of
          Nothing    -> pure ((Just i, rhs), (i, str))
          Just !post -> pure ((Just i, (VarE '(>>)) `AppE` post `AppE` rhs), (i, str))

      !m    =  M.fromList ((Nothing, maybe (VarE 'empty) id fallback) : branches)
      !trie = compileTrie strings
  in (m , trie)

-- | Switch expression with an optional first argument for performing a post-processing
--   action after every successful branch matching.
switch' :: Maybe (Q Exp) -> Q Exp -> Q Exp
switch' postAction exp = do
  !postAction <- sequence postAction
  (!cases, !fallback) <- parseSwitch exp
  genTrie $! genSwitchTrie' postAction cases fallback

switch :: Q Exp -> Q Exp
switch = switch' Nothing
