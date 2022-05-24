{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import FlatParse.Basic
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Word
import Data.Int
import Data.Bits

main :: IO ()
main = hspec $ do
  basicSpec

--------------------------------------------------------------------------------
-- Some combinators that make it easier to assert the results of a parser.

-- | The parser should parse this string, consuming it entirely, and succeed.
shouldParse :: Show e => Parser e a -> ByteString -> Expectation
p `shouldParse` s = case runParser p s of
  OK _ "" -> pure ()
  OK _ lo -> assertFailure $ "Unexpected leftover: " ++ show lo
  Fail -> assertFailure "Parse failed unexpectedly"
  Err e -> assertFailure $ "Parse threw unexpected error: " ++ show e

-- | The parser should parse this string, consuming it entirely, and succeed
-- yielding the matching value.
shouldParseWith ::
  (Show a, Eq a, Show e) => Parser e a -> (ByteString, a) -> Expectation
p `shouldParseWith` (s, r) = case runParser p s of
  OK r' "" -> r' `shouldBe` r
  OK _ lo -> assertFailure $ "Unexpected leftover: " ++ show lo
  Fail -> assertFailure "Parse failed unexpectedly"
  Err e -> assertFailure $ "Parse threw unexpected error: " ++ show e

-- | The parser should fail when given this string.
shouldParseFail :: Show e => Parser e a -> ByteString -> Expectation
p `shouldParseFail` s = case runParser p s of
  Fail -> pure ()
  OK _ _ -> assertFailure "Parse succeeded unexpectedly"
  Err e -> assertFailure $ "Parse threw unexpected error: " ++ show e

-- | The parser should throw an error when given this string.
shouldParseErr :: Parser e a -> ByteString -> Expectation
p `shouldParseErr` s = case runParser p s of
  Err e -> pure ()
  Fail -> assertFailure "Parse failed unexpectedly"
  OK _ _ -> assertFailure "Parse succeeded unexpectedly"

-- | The parser should throw an error when given this string, and the error
-- should be the one given.
shouldParseErrWith ::
  (Show e, Eq e) => Parser e a -> (ByteString, e) -> Expectation
p `shouldParseErrWith` (s, e) = case runParser p s of
  Err e' -> e' `shouldBe` e
  Fail -> assertFailure "Parse failed unexpectedly"
  OK _ _ -> assertFailure "Parse succeeded unexpectedly"

-- | The spec for FlatParse.Basic.
basicSpec :: SpecWith ()
basicSpec = describe "FlatParse.Basic" $ do
  describe "Errors and failures" $ do
    describe "empty" $
      it "always fails" $ empty `shouldParseFail` ""

    describe "err" $
      it "throws an error" $ err "nope" `shouldParseErr` ""

    describe "lookahead" $
      it "restores state" $ do
        let p = lookahead $(string "fun") *> $(string "function")
        p `shouldParse` "function"

    describe "fails" $ do
      it "expects child to fail" $ fails empty `shouldParse` ""
      it "fails when child succeeds" $ fails (pure ()) `shouldParseFail` ""
      it "propagates errors" $ fails (err "nope") `shouldParseErr` ""

    describe "try" $
      it "turns error into failure" $ try (err "nope") `shouldParseFail` ""

    describe "optional" $ do
      it "can succeed" $ optional (pure ()) `shouldParseWith` ("", Just ())
      it "can succeed when argument missing" $
        optional empty `shouldParseWith` ("", Nothing)
      it "propagates errors" $ optional (err "nope") `shouldParseErr` ""

    describe "optional_" $ do
      it "can succeed" $ optional (pure ()) `shouldParse` ""
      it "can succeed when argument missing" $ optional empty `shouldParse` ""
      it "propagates errors" $ optional (err "nope") `shouldParseErr` ""

    describe "optioned" $ do
      let opt p = optioned p (pure . reverse) (pure "bar")
      it "handles success" $ opt (pure "foo") `shouldParseWith` ("", "oof")
      it "handles failure" $ opt empty `shouldParseWith` ("", "bar")
      it "handles error" $ opt (err "nope") `shouldParseErr` ""

    describe "cut" $ do
      it "turns failure into error" $ empty `cut` "nope" `shouldParseErr` ""
      it "leaves success alone" $ pure () `cut` "nope" `shouldParse` ""
      it "propagates error" $
        err "inner" `cut` "outer" `shouldParseErrWith` ("", "inner")

    describe "cutting" $ do
      it "turns failure into error" $
        cutting empty "nope" (++) `shouldParseErrWith` ("", "nope")
      it "leaves success alone" $ do
        cutting (pure ()) "nope" (++) `shouldParse` ""
      it "combines errors" $
        cutting (err "!!!") "nope" (++) `shouldParseErrWith` ("", "!!!nope")

  describe "Basic lexing and parsing" $ do
    describe "eof" $ do
      it "succeeds at end of file" $ eof `shouldParse` ""
      it "fails with more input" $ eof `shouldParseFail` "more"

    describe "char" $ do
      it "succeeds on that char" $ $(char 'a') `shouldParse` "a"
      it "succeeds on multibyte char" $ $(char 'ȩ') `shouldParse` packUTF8 "ȩ"
      it "fails on the wrong char" $ $(char 'a') `shouldParseFail` "b"
      it "fails at end of file" $ $(char 'a') `shouldParseFail` ""

    describe "byte" $ do
      it "succeeds on that byte" $ byte 0x61 `shouldParse` "\x61"
      it "succeeds on high bytes" $ byte 0xfe `shouldParse` "\xfe"
      it "fails on the wrong byte" $ byte 0x61 `shouldParseFail` "\x62"
      it "fails on end of file" $ byte 0x61 `shouldParseFail` ""

    describe "bytes" $ do
      it "succeeds on those bytes" $
        $(bytes [1, 2, 3, 4]) `shouldParse` "\x01\x02\x03\x04"
      it "succeeds on high bytes" $
        $(bytes [0xf1, 0xf2, 0xf3, 0xf4]) `shouldParse` "\xf1\xf2\xf3\xf4"
      it "fails on wrong bytes" $
        $(bytes [1, 2, 5, 4]) `shouldParseFail` "\x01\x02\x03\x04"
      it "fails when out of space" $
        $(bytes [1, 2, 3, 4]) `shouldParseFail` "\x01\x02\x03"

    describe "string" $ do
      it "succeeds on the right string" $ $(string "foo") `shouldParse` "foo"
      it "succeeds with multibyte chars" $
        $(string "foȩ") `shouldParse` packUTF8 "foȩ"
      it "fails on the wrong string" $ $(string "foo") `shouldParseFail` "bar"
      it "fails when out of space" $ $(string "foo") `shouldParseFail` "fo"

    describe "switch" $ do
      pure ()

    describe "switchWithPost" $ do
      pure ()

    describe "rawSwitchWithPost" $ do
      pure ()

    describe "satisfy" $ do
      pure ()

    describe "satisfyASCII" $ do
      pure ()

    describe "satisfyASCII_" $ do
      pure ()

    describe "fusedSatisfy" $ do
      pure ()

    describe "anyWord8" $ do
      pure ()

    describe "anyWord16" $ do
      pure ()

    describe "anyWord32" $ do
      pure ()

    describe "anyWord" $ do
      pure ()

    describe "anyChar" $ do
      pure ()

    describe "anyChar_" $ do
      pure ()

    describe "anyCharASCII" $ do
      pure ()

    describe "anyCharASCII_" $ do
      pure ()

    describe "isDigit" $ do
      pure ()

    describe "isGreekLetter" $ do
      pure ()

    describe "isLatinLetter" $ do
      pure ()

    describe "readInt" $ do
      pure ()

    describe "readInteger" $ do
      pure ()

    describe "Explicit-endianness machine integers" $ do
      describe "Unsigned" $ do
        prop "parses Word8s" $ do
          \(w :: Word8)  -> anyWord8    `shouldParseWith` (w8AsByteString w, w)
        prop "parses Word16s (LE)" $ do
          \(w :: Word16) -> anyWord16le `shouldParseWith` (w16leAsByteString w, w)
        prop "parses Word16s (BE)" $ do
          \(w :: Word16) -> anyWord16be `shouldParseWith` (B.reverse (w16leAsByteString w), w)
        prop "parses Word32s (LE)" $ do
          \(w :: Word32) -> anyWord32le `shouldParseWith` (w32leAsByteString w, w)
        prop "parses Word32s (BE)" $ do
          \(w :: Word32) -> anyWord32be `shouldParseWith` (B.reverse (w32leAsByteString w), w)
        prop "parses Word64s (LE)" $ do
          \(w :: Word64) -> anyWord64le `shouldParseWith` (w64leAsByteString w, w)
        prop "parses Word64s (BE)" $ do
          \(w :: Word64) -> anyWord64be `shouldParseWith` (B.reverse (w64leAsByteString w), w)

      describe "Signed" $ do
        prop "parses Int8s" $ do
          \(i :: Int8)   -> anyInt8     `shouldParseWith` (w8AsByteString i, i)
        prop "parses Int16s (LE)" $ do
          \(i :: Int16)  -> anyInt16le  `shouldParseWith` (w16leAsByteString i, i)
        prop "parses Int16s (BE)" $ do
          \(i :: Int16)  -> anyInt16be  `shouldParseWith` (B.reverse (w16leAsByteString i), i)
        prop "parses Int32s (LE)" $ do
          \(i :: Int32)  -> anyInt32le  `shouldParseWith` (w32leAsByteString i, i)
        prop "parses Int32s (BE)" $ do
          \(i :: Int32)  -> anyInt32be  `shouldParseWith` (B.reverse (w32leAsByteString i), i)
        prop "parses Int64s (LE)" $ do
          \(i :: Int64)  -> anyInt64le  `shouldParseWith` (w64leAsByteString i, i)
        prop "parses Int64s (BE)" $ do
          \(i :: Int64)  -> anyInt64be  `shouldParseWith` (B.reverse (w64leAsByteString i), i)

  describe "Combinators" $ do
    describe "(<|>)" $ do
      pure ()

    describe "branch" $ do
      pure ()

    describe "chainl" $ do
      pure ()

    describe "chainr" $ do
      pure ()

    describe "many" $ do
      pure ()

    describe "many_" $ do
      pure ()

    describe "some" $ do
      pure ()

    describe "some_" $ do
      pure ()

    describe "notFollowedBy" $ do
      pure ()

  describe "Positions and spans" $ do
    describe "Pos Ord instance" $ do
      pure ()

    describe "getPos" $ do
      pure ()

    describe "setPos" $ do
      pure ()

    describe "endPos" $ do
      pure ()

    describe "spanOf" $ do
      pure ()

    describe "spanned" $ do
      pure ()

    describe "byteStringOf" $ do
      pure ()

    describe "byteStringed" $ do
      pure ()

    describe "inSpan" $ do
      pure ()

  describe "Positions and span conversions" $ do
    describe "validPos" $ do
      pure ()

    describe "posLineCols" $ do
      pure ()

    describe "unsafeSpanToByteString" $ do
      pure ()

    describe "unsafeSlice" $ do
      pure ()

    describe "mkPos" $ do
      pure ()

    describe "lines" $ do
      pure ()

  describe "Getting the rest of the input" $ do
    describe "takeLine" $ do
      pure ()

    describe "traceLine" $ do
      pure ()

    describe "takeRest" $ do
      pure ()

    describe "traceRest" $ do
      pure ()

  describe "String conversions" $ do
    describe "packUTF8" $ do
      pure ()

    describe "unpackUTF8" $ do
      pure ()

--------------------------------------------------------------------------------

w8AsByteString :: (Bits w, Num w, Integral w) => w -> ByteString
w8AsByteString w = B.pack [b1]
  where
    b1 = fromIntegral $  w .&. 0x00FF

w16leAsByteString :: (Bits w, Num w, Integral w) => w -> ByteString
w16leAsByteString w = B.pack [b1, b2]
  where
    b1 = fromIntegral $  w .&. 0x00FF
    b2 = fromIntegral $ (w .&. 0xFF00) `shiftR` 8

w32leAsByteString :: (Bits w, Num w, Integral w) => w -> ByteString
w32leAsByteString w = B.pack [b1, b2, b3, b4]
  where
    b1 = fromIntegral $  w .&. 0x000000FF
    b2 = fromIntegral $ (w .&. 0x0000FF00) `shiftR` 8
    b3 = fromIntegral $ (w .&. 0x00FF0000) `shiftR` 16
    b4 = fromIntegral $ (w .&. 0xFF000000) `shiftR` 24

w64leAsByteString :: (Bits w, Num w, Integral w) => w -> ByteString
w64leAsByteString w = B.pack [b1, b2, b3, b4, b5, b6, b7, b8]
  where
    b1 = fromIntegral $  w .&. 0x00000000000000FF
    b2 = fromIntegral $ (w .&. 0x000000000000FF00) `shiftR` 8
    b3 = fromIntegral $ (w .&. 0x0000000000FF0000) `shiftR` 16
    b4 = fromIntegral $ (w .&. 0x00000000FF000000) `shiftR` 24
    b5 = fromIntegral $ (w .&. 0x000000FF00000000) `shiftR` 32
    b6 = fromIntegral $ (w .&. 0x0000FF0000000000) `shiftR` 40
    b7 = fromIntegral $ (w .&. 0x00FF000000000000) `shiftR` 48
    b8 = fromIntegral $ (w .&. 0xFF00000000000000) `shiftR` 56
