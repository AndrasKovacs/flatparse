{-# language UnboxedTuples #-}

module FlatParse.Basic.Strings where

import Prelude hiding ( getChar )

import FlatParse.Basic.Parser
import FlatParse.Basic.Bytes ( getBytesOf )

import FlatParse.Common.Assorted ( derefChar8# )

import GHC.Exts

import Language.Haskell.TH
import qualified FlatParse.Common.Numbers as Common
import qualified FlatParse.Common.Assorted as Common

-- | Parse a UTF-8 `Char` for which a predicate holds.
satisfy :: (Char -> Bool) -> Parser e Char
satisfy f = Parser \fp eob s -> case runParser# getChar fp eob s of
  OK# c s | f c -> OK# c s
  _             -> Fail#
{-#  inline satisfy #-}

-- | Skip a UTF-8 `Char` for which a predicate holds.
satisfy_ :: (Char -> Bool) -> Parser e ()
satisfy_ f = Parser \fp eob s -> case runParser# getChar fp eob s of
  OK# c s | f c -> OK# () s
  _             -> Fail#
{-#  inline satisfy_ #-}

-- | Parse an ASCII `Char` for which a predicate holds. Assumption: the predicate must only return
--   `True` for ASCII-range characters. Otherwise this function might read a 128-255 range byte,
--   thereby breaking UTF-8 decoding.
satisfyASCII :: (Char -> Bool) -> Parser e Char
satisfyASCII f = Parser \fp eob s -> case eqAddr# eob s of
  1# -> Fail#
  _  -> case derefChar8# s of
    c1 | f (C# c1) -> OK# (C# c1) (plusAddr# s 1#)
       | otherwise -> Fail#
{-#  inline satisfyASCII #-}

-- | Skip an ASCII `Char` for which a predicate holds. Assumption: the predicate
--   must only return `True` for ASCII-range characters.
satisfyASCII_ :: (Char -> Bool) -> Parser e ()
satisfyASCII_ f = Parser \fp eob s -> case eqAddr# eob s of
  1# -> Fail#
  _  -> case derefChar8# s of
    c1 | f (C# c1) -> OK# () (plusAddr# s 1#)
       | otherwise -> Fail#
{-#  inline satisfyASCII_ #-}

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
fusedSatisfy :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Parser e Char
fusedSatisfy f1 f2 f3 f4 = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# | f1 (C# c1) -> OK# (C# c1) (plusAddr# buf 1#)
         | otherwise  -> Fail#
      _  -> case eqAddr# eob (plusAddr# buf 1#) of
        1# -> Fail#
        _ -> case indexCharOffAddr# buf 1# of
          c2 -> case c1 `leChar#` '\xDF'# of
            1# ->
              let resc = C# (chr# (((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#) `orI#`
                                   (ord# c2 -# 0x80#)))
              in case f2 resc of
                   True -> OK# resc (plusAddr# buf 2#)
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
                         True -> OK# resc (plusAddr# buf 3#)
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
                             True -> OK# resc (plusAddr# buf 4#)
                             _    -> Fail#
{-# inline fusedSatisfy #-}

-- | Skipping variant of `fusedSatisfy`.
fusedSatisfy_ :: (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Parser e ()
fusedSatisfy_ f1 f2 f3 f4 = () <$ fusedSatisfy f1 f2 f3 f4
{-# inline fusedSatisfy_ #-}

-- | Parse any UTF-8-encoded `Char`.
getChar :: Parser e Char
getChar = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# (C# c1) (plusAddr# buf 1#)
      _  -> case eqAddr# eob (plusAddr# buf 1#) of
        1# -> Fail#
        _ -> case indexCharOffAddr# buf 1# of
          c2 -> case c1 `leChar#` '\xDF'# of
            1# ->
              let resc = ((ord# c1 -# 0xC0#) `uncheckedIShiftL#` 6#) `orI#`
                          (ord# c2 -# 0x80#)
              in OK# (C# (chr# resc)) (plusAddr# buf 2#)
            _ -> case eqAddr# eob (plusAddr# buf 2#) of
              1# -> Fail#
              _  -> case indexCharOffAddr# buf 2# of
                c3 -> case c1 `leChar#` '\xEF'# of
                  1# ->
                    let resc = ((ord# c1 -# 0xE0#) `uncheckedIShiftL#` 12#) `orI#`
                               ((ord# c2 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                (ord# c3 -# 0x80#)
                    in OK# (C# (chr# resc)) (plusAddr# buf 3#)
                  _ -> case eqAddr# eob (plusAddr# buf 3#) of
                    1# -> Fail#
                    _  -> case indexCharOffAddr# buf 3# of
                      c4 ->
                        let resc = ((ord# c1 -# 0xF0#) `uncheckedIShiftL#` 18#) `orI#`
                                   ((ord# c2 -# 0x80#) `uncheckedIShiftL#` 12#) `orI#`
                                   ((ord# c3 -# 0x80#) `uncheckedIShiftL#`  6#) `orI#`
                                    (ord# c4 -# 0x80#)
                        in OK# (C# (chr# resc)) (plusAddr# buf 4#)
{-# inline getChar #-}

-- | Skip any UTF-8-encoded `Char`.
getChar_ :: Parser e ()
getChar_ = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# () (plusAddr# buf 1#)
      _  ->
        let buf' =
              case c1 `leChar#` '\xDF'# of
                1# -> plusAddr# buf 2#
                _  -> case c1 `leChar#` '\xEF'# of
                    1# -> plusAddr# buf 3#
                    _ ->  plusAddr# buf 4#
        in case leAddr# buf' eob of
             1# -> OK# () buf'
             _  -> Fail#
{-# inline getChar_ #-}

-- | Parse any `Char` in the ASCII range, fail if the next input character is not in the range.
--   This is more efficient than `getChar` if we are only working with ASCII.
getCharASCII :: Parser e Char
getCharASCII = Parser \fp eob buf -> case eqAddr# eob buf of
  1# -> Fail#
  _  -> case derefChar8# buf of
    c1 -> case c1 `leChar#` '\x7F'# of
      1# -> OK# (C# c1) (plusAddr# buf 1#)
      _  -> Fail#
{-# inline getCharASCII #-}

-- | Skip any `Char` in the ASCII range. More efficient than `getChar_` if we're working only with
--   ASCII.
getCharASCII_ :: Parser e ()
getCharASCII_ = () <$ getCharASCII
{-# inline getCharASCII_ #-}

-- | Parse a UTF-8 character literal. This is a template function, you can use it as
--   @$(char \'x\')@, for example, and the splice in this case has type @Parser e ()@.
getCharOf :: Char -> Q Exp
getCharOf c = getStringOf [c]

-- | Parse a UTF-8 string literal. This is a template function, you can use it as @$(string "foo")@,
--   for example, and the splice has type @Parser e ()@.
getStringOf :: String -> Q Exp
getStringOf str = getBytesOf (Common.strToBytes str)

-- | Read a non-negative `Int` from the input, as a non-empty digit sequence.
-- The `Int` may overflow in the result.
getAsciiDecimalInt :: Parser e Int
getAsciiDecimalInt = Parser \fp eob s -> case Common.readInt eob s of
  (# (##) | #)        -> Fail#
  (# | (# n, s' #) #) -> OK# (I# n) s'
{-# inline getAsciiDecimalInt #-}

-- | Read an `Int` from the input, as a non-empty case-insensitive ASCII
--   hexadecimal digit sequence. The `Int` may overflow in the result.
getAsciiHexInt :: Parser e Int
getAsciiHexInt = Parser \fp eob s -> case Common.readIntHex eob s of
  (# (##) | #)        -> Fail#
  (# | (# n, s' #) #) -> OK# (I# n) s'
{-# inline getAsciiHexInt #-}

-- | Read a non-negative `Integer` from the input, as a non-empty digit
-- sequence.
getAsciiDecimalInteger :: Parser e Integer
getAsciiDecimalInteger = Parser \fp eob s -> case Common.readInteger fp eob s of
  (# (##) | #)        -> Fail#
  (# | (# i, s' #) #) -> OK# i s'
{-# inline getAsciiDecimalInteger #-}
