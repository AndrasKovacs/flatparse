{-# LANGUAGE UnboxedTuples #-}

-- | Parsers and utilities for parsing (UTF-8) 'Char's and 'String's.

module FlatParse.Basic.Strings where

import FlatParse.Basic.Parser
import FlatParse.Basic.Bytes ( bytes )

import FlatParse.Common.GHCExts

import Language.Haskell.TH
import qualified FlatParse.Common.Numbers as Common
import qualified FlatParse.Common.Assorted as Common

-- | Parse a UTF-8 `Char` for which a predicate holds.
satisfy :: (Char -> Bool) -> ParserT st e Char
satisfy f = ParserT \fp eob s st -> case runParserT# anyChar fp eob s st of
  OK# st' c s | f c -> OK#   st' c s
  (# st', _ #)      -> Fail# st'
{-# inline satisfy #-}

-- | Skip a UTF-8 `Char` for which a predicate holds.
skipSatisfy :: (Char -> Bool) -> ParserT st e ()
skipSatisfy f = ParserT \fp eob s st -> case runParserT# anyChar fp eob s st of
  OK# st' c s | f c -> OK#   st' () s
  (# st', _ #)      -> Fail# st'
{-#  inline skipSatisfy #-}

-- | Parse an ASCII `Char` for which a predicate holds. Assumption: the predicate must only return
--   `True` for ASCII-range characters. Otherwise this function might read a 128-255 range byte,
--   thereby breaking UTF-8 decoding.
satisfyAscii :: (Char -> Bool) -> ParserT st e Char
satisfyAscii f = ParserT \fp eob s st -> case eqAddr# eob s of
  1# -> Fail# st
  _  -> case Common.derefChar8# s of
    c1 | f (C# c1) -> OK#   st (C# c1) (plusAddr# s 1#)
       | otherwise -> Fail# st
{-#  inline satisfyAscii #-}

-- | Skip an ASCII `Char` for which a predicate holds. Assumption: the predicate
--   must only return `True` for ASCII-range characters.
satisfyAscii_ :: (Char -> Bool) -> ParserT st e ()
satisfyAscii_ f = ParserT \fp eob s st -> case eqAddr# eob s of
  1# -> Fail# st
  _  -> case Common.derefChar8# s of
    c1 | f (C# c1) -> OK#   st () (plusAddr# s 1#)
       | otherwise -> Fail# st
{-#  inline satisfyAscii_ #-}

-- | This is a variant of `satisfy` which allows more optimization. We can pick four testing
--   functions for the four cases for the possible number of bytes in the UTF-8 character. So in
--   @fusedSatisfy f1 f2 f3 f4@, if we read a one-byte character, the result is scrutinized with
--   @f1@, for two-bytes, with @f2@, and so on. This can result in dramatic lexing speedups.
--
--   For example, if we want to accept any letter, the naive solution would be to use
--   `Data.Char.isLetter`, but this accesses a large lookup table of Unicode character classes. We
--   can do better with @fusedSatisfy isLatinLetter isLetter isLetter isLetter@, since here the
--   `isLatinLetter` is inlined into the UTF-8 decoding, and it probably handles a great majority of
--   all cases without accessing the character table.
fusedSatisfy :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> ParserT st e Char
fusedSatisfy f1 f2 f3 f4 = ParserT \fp eob buf st ->
    case eqAddr# eob buf of
      1# -> Fail# st
      _  -> case Common.derefChar8# buf of
        c1 -> case c1 `leChar#` '\x7F'# of
          1# | f1 (C# c1) -> OK#   st (C# c1) (plusAddr# buf 1#)
             | otherwise  -> Fail# st
          _  -> case eqAddr# eob (plusAddr# buf 1#) of
            1# -> Fail# st
            _ -> case indexCharOffAddr# buf 1# of
              c2 -> case c1 `leChar#` '\xDF'# of
                1# ->
                  let resc = C# (chr# (((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#) `orI#`
                                       (ord# c2 -# 0x80#)))
                  in case f2 resc of
                       True -> OK#   st resc (plusAddr# buf 2#)
                       _    -> Fail# st
                _ -> case eqAddr# eob (plusAddr# buf 2#) of
                  1# -> Fail# st
                  _  -> case indexCharOffAddr# buf 2# of
                    c3 -> case c1 `leChar#` '\xEF'# of
                      1# ->
                        let resc = C# (chr# (((ord# c1 -# 0xE0#) `uncheckedIShiftL#` 12#) `orI#`
                                             ((ord# c2 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                             (ord# c3 -# 0x80#)))
                        in case f3 resc of
                             True -> OK#   st resc (plusAddr# buf 3#)
                             _    -> Fail# st
                      _ -> case eqAddr# eob (plusAddr# buf 3#) of
                        1# -> Fail# st
                        _  -> case indexCharOffAddr# buf 3# of
                          c4 ->
                            let resc = C# (chr# (((ord# c1 -# 0xF0#) `uncheckedIShiftL#` 18#) `orI#`
                                                 ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 12#) `orI#`
                                                 ((ord# c3 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                                  (ord# c4 -# 0x80#)))
                            in case f4 resc of
                                 True -> OK#   st resc (plusAddr# buf 4#)
                                 _    -> Fail# st
{-# inline fusedSatisfy #-}

-- | Skipping variant of `fusedSatisfy`.
fusedSatisfy_ :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Parser e ()
fusedSatisfy_ f1 f2 f3 f4 = () <$ fusedSatisfy f1 f2 f3 f4
{-# inline fusedSatisfy_ #-}

-- | Parse any single Unicode character encoded using UTF-8 as a 'Char'.
anyChar :: ParserT st e Char
anyChar = ParserT \fp eob buf st -> case eqAddr# eob buf of
  1# -> Fail# st
  _  -> case Common.derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# st (C# c1) (plusAddr# buf 1#)
      _  -> case eqAddr# eob (plusAddr# buf 1#) of
        1# -> Fail# st
        _ -> case indexCharOffAddr# buf 1# of
          c2 -> case c1 `leChar#` '\xDF'# of
            1# ->
              let resc = ((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#) `orI#`
                          (ord# c2 -# 0x80#)
              in OK# st (C# (chr# resc)) (plusAddr# buf 2#)
            _ -> case eqAddr# eob (plusAddr# buf 2#) of
              1# -> Fail# st
              _  -> case indexCharOffAddr# buf 2# of
                c3 -> case c1 `leChar#` '\xEF'# of
                  1# ->
                    let resc = ((ord# c1 -# 0xE0#) `uncheckedIShiftL#` 12#) `orI#`
                               ((ord# c2 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                (ord# c3 -# 0x80#)
                    in OK# st (C# (chr# resc)) (plusAddr# buf 3#)
                  _ -> case eqAddr# eob (plusAddr# buf 3#) of
                    1# -> Fail# st
                    _  -> case indexCharOffAddr# buf 3# of
                      c4 ->
                        let resc = ((ord# c1 -# 0xF0#) `uncheckedIShiftL#` 18#) `orI#`
                                   ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 12#) `orI#`
                                   ((ord# c3 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                    (ord# c4 -# 0x80#)
                        in OK# st (C# (chr# resc)) (plusAddr# buf 4#)
{-# inline anyChar #-}

-- | Skip any single Unicode character encoded using UTF-8.
anyChar_ :: ParserT st e ()
anyChar_ = ParserT \fp eob buf st -> case eqAddr# eob buf of
  1# -> Fail# st
  _  -> case Common.derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# st () (plusAddr# buf 1#)
      _  ->
        let buf' =
              case c1 `leChar#` '\xDF'# of
                1# -> plusAddr# buf 2#
                _  -> case c1 `leChar#` '\xEF'# of
                    1# -> plusAddr# buf 3#
                    _ ->  plusAddr# buf 4#
        in case leAddr# buf' eob of
             1# -> OK#   st () buf'
             _  -> Fail# st
{-# inline anyChar_ #-}

-- | Parse any single ASCII character (a single byte) as a 'Char'.
--
-- More efficient than 'anyChar' for ASCII-only input.
anyAsciiChar :: ParserT st e Char
anyAsciiChar = ParserT \fp eob buf st -> case eqAddr# eob buf of
  1# -> Fail# st
  _  -> case Common.derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK#   st (C# c1) (plusAddr# buf 1#)
      _  -> Fail# st
{-# inline anyAsciiChar #-}

-- | Skip any single ASCII character (a single byte).
--
-- More efficient than 'anyChar_' for ASCII-only input.
anyAsciiChar_ :: ParserT st e ()
anyAsciiChar_ = () <$ anyAsciiChar
{-# inline anyAsciiChar_ #-}

--------------------------------------------------------------------------------

-- | Parse a non-empty ASCII decimal digit sequence as a 'Word'.
--   Fails on overflow.
anyAsciiDecimalWord :: ParserT st e Word
anyAsciiDecimalWord = ParserT \fp eob s st ->
    case Common.anyAsciiDecimalWord# eob s of
      (# | (# w, s' #) #) -> OK#   st (W# w) s'
      (# (##) | #)        -> Fail# st
{-# inline anyAsciiDecimalWord #-}

-- | Parse a non-empty ASCII decimal digit sequence as a positive 'Int'.
--   Fails on overflow.
anyAsciiDecimalInt :: ParserT st e Int
anyAsciiDecimalInt = ParserT \fp eob s st ->
    case Common.anyAsciiDecimalInt# eob s of
      (# | (# n, s' #) #) -> OK#   st (I# n) s'
      (# (##) | #)        -> Fail# st
{-# inline anyAsciiDecimalInt #-}

-- | Parse a non-empty ASCII decimal digit sequence as a positive 'Integer'.
anyAsciiDecimalInteger :: ParserT st e Integer
anyAsciiDecimalInteger = ParserT \fp eob s st ->
    case Common.anyAsciiDecimalInteger# fp eob s of
      (# | (# i, s' #) #) -> OK#   st i s'
      (# (##) | #)        -> Fail# st
{-# inline anyAsciiDecimalInteger #-}

-- | Parse a non-empty, case-insensitive ASCII hexadecimal digit sequence as a
--   'Word'.
--   Fails on overflow.
anyAsciiHexWord :: ParserT st e Word
anyAsciiHexWord = ParserT \fp eob s st ->
    case Common.anyAsciiHexWord# eob s of
      (# | (# w, s' #) #) -> OK#   st (W# w) s'
      (# (##) | #)        -> Fail# st
{-# inline anyAsciiHexWord #-}

-- | Parse a non-empty, case-insensitive ASCII hexadecimal digit sequence as a
--   positive 'Int'.
--   Fails on overflow.
anyAsciiHexInt :: ParserT st e Int
anyAsciiHexInt = ParserT \fp eob s st ->
    case Common.anyAsciiHexInt# eob s of
      (# | (# n, s' #) #) -> OK#   st (I# n) s'
      (# (##) | #)        -> Fail# st
{-# inline anyAsciiHexInt #-}

--------------------------------------------------------------------------------

-- | Parse a UTF-8 character literal. This is a template function, you can use it as
--   @$(char \'x\')@, for example, and the splice in this case has type @Parser e ()@.
char :: Char -> Q Exp
char c = string [c]

-- | Parse a UTF-8 string literal. This is a template function, you can use it as @$(string "foo")@,
--   for example, and the splice has type @Parser e ()@.
string :: String -> Q Exp
string str = bytes (Common.strToBytes str)
