{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Numeric (showHex)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Char
import FlatParse.Basic
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ( (.&.) )
import Data.Word
import Data.Int
import Data.Bits
import Test.QuickCheck.Instances.ByteString()

main :: IO ()
main = hspec $ do
  basicSpec

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

    describe "withOption" $ do
      let opt p = withOption p (pure . reverse) (pure "bar")
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

    describe "skip" $ do
      prop "skips to the end of input" $
        \(bs :: ByteString) ->
          (skip (B.length bs) >> eof) `shouldParse` bs

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
      it "parses simple words" $
        $( switch
             [|
               case _ of
                 "foo" -> pure 1
                 "bar" -> pure 2
               |]
         )
          `shouldParseWith` ("foo", 1)

      it "matches the default" $
        $( switch
             [|
               case _ of
                 "foo" -> pure 1
                 "bar" -> pure 2
                 _ -> pure 0
               |]
         )
          `shouldParsePartialWith` ("fez", 0)

      it "fails with no default" $
        $( switch
             [|
               case _ of
                 "foo" -> pure 1
                 "bar" -> pure 2
               |]
         )
          `shouldParseFail` "fez"

      it "prefers longest match" $
        $( switch
             [|
               case _ of
                 "foo" -> pure 1
                 "foobar" -> pure 2
               |]
         )
          `shouldParseWith` ("foobar", 2)

      it "doesn't reproduce bug #12" $
        $( switch
             [|
               case _ of
                 "Eac" -> pure ()
                 "EAc" -> pure ()
                 "E" -> pure ()
               |]
         )
          `shouldParse` "E"

    describe "switchWithPost" $ do
      it "applies post after match" $
        $( switchWithPost
             (Just [|$(string "bar")|])
             [|
               case _ of
                 "foo" -> pure ()
               |]
         )
          `shouldParse` "foobar"

      it "doesn't apply post after default" $
        $( switchWithPost
             (Just [|$(string "bar")|])
             [|
               case _ of
                 "foo" -> pure ()
                 _ -> pure ()
               |]
         )
          `shouldParse` ""

      it "requires the post must match" $
        $( switchWithPost
             (Just [|$(string "bar")|])
             [|
               case _ of
                 "foo" -> pure ()
               |]
         )
          `shouldParseFail` "foo"

    describe "rawSwitchWithPost" $ do
      it "parses simple words" $
        $( rawSwitchWithPost
             Nothing
             [ ("foo", [|pure 1|]),
               ("bar", [|pure 2|])
             ]
             Nothing
         )
          `shouldParseWith` ("foo", 1)

      it "matches the default" $
        $( rawSwitchWithPost
             Nothing
             [ ("foo", [|pure 1|]),
               ("bar", [|pure 2|])
             ]
             (Just [|pure 0|])
         )
          `shouldParsePartialWith` ("fez", 0)

      it "fails with no default" $
        $( rawSwitchWithPost
             Nothing
             [ ("foo", [|pure 1|]),
               ("bar", [|pure 2|])
             ]
             Nothing
         )
          `shouldParseFail` "fez"

      it "prefers longest match" $
        $( rawSwitchWithPost
             Nothing
             [ ("foo", [|pure 1|]),
               ("foobar", [|pure 2|])
             ]
             Nothing
         )
          `shouldParseWith` ("foobar", 2)

      it "applies post after match" $
        $( rawSwitchWithPost
             (Just [|$(string "bar")|])
             [("foo", [|pure ()|])]
             Nothing
         )
          `shouldParse` "foobar"

      it "doesn't apply post after default" $
        $( rawSwitchWithPost
             (Just [|$(string "bar")|])
             [("foo", [|pure ()|])]
             (Just [|pure ()|])
         )
          `shouldParse` ""

      it "requires the post must match" $
        $( rawSwitchWithPost
             (Just [|$(string "bar")|])
             [("foo", [|pure ()|])]
             Nothing
         )
          `shouldParseFail` "foo"

    describe "satisfy" $ do
      it "succeeds on the right char" $
        satisfy (== 'a') `shouldParseWith` ("a", 'a')

      it "succeeds on multi-byte chars" $ do
        let chars = "$¢€𐍈" :: [Char]
        sequence_
          [ if a == b
              then satisfy (== a) `shouldParseWith` (packUTF8 (pure b), b)
              else satisfy (== a) `shouldParseFail` packUTF8 (pure b)
            | a <- chars,
              b <- chars
          ]

      it "fails on the wrong char" $
        satisfy (== 'a') `shouldParseFail` "b"
      it "fails at end of file" $
        satisfy (== 'a') `shouldParseFail` ""

    describe "satisfyASCII" $ do
      it "succeeds on the right char" $
        satisfyASCII (== 'a') `shouldParseWith` ("a", 'a')
      it "fails on the wrong char" $
        satisfyASCII (== 'a') `shouldParseFail` "b"

      it "fails on the wrong multi-byte char" $
        -- The specification for satisfyASCII requires that the predicate
        -- return False for non-ASCII characters, but multi-byte chars are
        -- still allowed in the input.
        satisfyASCII (== 'a') `shouldParseFail` packUTF8 "ȩ"

      it "fails at end of file" $
        satisfyASCII (== 'a') `shouldParseFail` ""

    describe "satisfyASCII_" $ do
      it "succeeds on the right char" $
        satisfyASCII_ (== 'a') `shouldParseWith` ("a", ())
      it "fails on the wrong char" $
        satisfyASCII_ (== 'a') `shouldParseFail` "b"
      it "fails on the wrong multi-byte char" $
        satisfyASCII_ (== 'a') `shouldParseFail` packUTF8 "ȩ"
      it "fails at end of file" $
        satisfyASCII_ (== 'a') `shouldParseFail` ""

    describe "fusedSatisfy" $ do
      it "correctly routes chars based on length" $ do
        fusedSatisfy (== '$') (const False) (const False) (const False)
          `shouldParse` packUTF8 "$"
        fusedSatisfy (const False) (== '¢') (const False) (const False)
          `shouldParse` packUTF8 "¢"
        fusedSatisfy (const False) (const False) (== '€') (const False)
          `shouldParse` packUTF8 "€"
        fusedSatisfy (const False) (const False) (const False) (== '𐍈')
          `shouldParse` packUTF8 "𐍈"

      it "fails on empty input" $
        fusedSatisfy (const True) (const True) (const True) (const True)
          `shouldParseFail` ""

    describe "anyWord8" $ do
      it "reads a byte" $ anyWord8 `shouldParseWith` ("\xef", 0xef)
      it "fails on empty input" $ anyWord8 `shouldParseFail` ""

    describe "anyWord16" $ do
      -- Byte order is unspecified, so just assert that it succeeds.
      it "succeeds" $ anyWord16 `shouldParse` "\xef\xbe"

      it "fails on empty input" $ anyWord16 `shouldParseFail` ""
      it "fails on insufficient input" $ anyWord16 `shouldParseFail` "\xff"

    describe "anyWord32" $ do
      -- Byte order is unspecified, so just assert that it succeeds.
      it "succeeds" $ anyWord32 `shouldParse` "\xef\xbe\xae\x7e"

      it "fails on empty input" $ anyWord32 `shouldParseFail` ""
      it "fails on insufficient input" $
        anyWord32 `shouldParseFail` "\xff\xff\xff"

    describe "anyWord" $ do
      -- This combinator is inherently non-portable, but we know a Word is at
      -- least some bytes.
      it "fails on empty input" $ anyWord `shouldParseFail` ""

    describe "anyChar" $ do
      it "reads 1-byte char" $ anyChar `shouldParseWith` (packUTF8 "$", '$')
      it "reads 2-byte char" $ anyChar `shouldParseWith` (packUTF8 "¢", '¢')
      it "reads 3-byte char" $ anyChar `shouldParseWith` (packUTF8 "€", '€')
      it "reads 4-byte char" $ anyChar `shouldParseWith` (packUTF8 "𐍈", '𐍈')
      it "fails on empty input" $ anyChar `shouldParseFail` ""

    describe "anyChar_" $ do
      it "reads 1-byte char" $ anyChar_ `shouldParseWith` (packUTF8 "$", ())
      it "reads 2-byte char" $ anyChar_ `shouldParseWith` (packUTF8 "¢", ())
      it "reads 3-byte char" $ anyChar_ `shouldParseWith` (packUTF8 "€", ())
      it "reads 4-byte char" $ anyChar_ `shouldParseWith` (packUTF8 "𐍈", ())
      it "fails on empty input" $ anyChar_ `shouldParseFail` ""

    describe "anyCharASCII" $ do
      it "reads ASCII char" $ anyCharASCII `shouldParseWith` (packUTF8 "$", '$')
      it "fails on non-ASCII char" $ anyCharASCII `shouldParseFail` packUTF8 "¢"
      it "fails on empty input" $ anyCharASCII `shouldParseFail` ""

    describe "anyCharASCII_" $ do
      it "reads ASCII char" $ anyCharASCII_ `shouldParseWith` (packUTF8 "$", ())
      it "fails on non-ASCII char" $
        anyCharASCII_ `shouldParseFail` packUTF8 "¢"
      it "fails on empty input" $ anyCharASCII_ `shouldParseFail` ""

    describe "isDigit" $ do
      it "agrees with Data.Char" $
        property $
          \c -> isDigit c === Data.Char.isDigit c

    describe "isLatinLetter" $ do
      it "agrees with Data.Char" $
        property $
          \c ->
            isLatinLetter c
              === (Data.Char.isAsciiUpper c || Data.Char.isAsciiLower c)

    describe "readInt" $ do
      it "round-trips on non-negative Ints" $
        property $
          \(NonNegative i) -> readInt `shouldParseWith` (packUTF8 (show i), i)

      it "fails on non-integers" $ readInt `shouldParseFail` "foo"
      it "fails on negative integers" $ readInt `shouldParseFail` "-5"
      it "fails on empty input" $ readInt `shouldParseFail` ""

    describe "readIntHex" $ do
      it "round-trips on non-negative Ints, lowercase" $
        property $
          \(NonNegative i) -> readIntHex `shouldParseWith` (packUTF8 (showHex i ""), i)

      it "round-trips on non-negative Ints, uppercase" $
        property $
          \(NonNegative i) -> readIntHex `shouldParseWith` (packUTF8 (Data.Char.toUpper <$> showHex i ""), i)

      it "fails on non-integers" $ readIntHex `shouldParseFail` "quux"
      it "fails on negative integers" $ readIntHex `shouldParseFail` "-5"
      it "fails on empty input" $ readIntHex `shouldParseFail` ""

    describe "readInteger" $ do
      it "round-trips on non-negative Integers" $
        property $
          \(NonNegative i) ->
            readInteger `shouldParseWith` (packUTF8 (show i), i)

      it "fails on non-integers" $ readInteger `shouldParseFail` "foo"
      it "fails on negative integers" $ readInteger `shouldParseFail` "-5"
      it "fails on empty input" $ readInteger `shouldParseFail` ""

    describe "anyCString" $ do
      prop "parses arbitrary null-terminated bytestrings" $
        \(bs :: ByteString) ->
          let bs' = B.snoc bs 0x00
              expected = B.takeWhile (/= 0x00) bs'
          in  anyCString `shouldParsePartialWith` (bs', expected)

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
    describe "Functor instance" $ do
      it "fmaps over the result" $
        ((+ 2) <$> readInt) `shouldParseWith` ("2", 4)

    describe "Applicative instance" $ do
      it "combines using <*>" $
        ((+) <$> readInt <* $(string "+") <*> readInt)
          `shouldParseWith` ("2+3", 5)

    describe "Monad instance" $ do
      it "combines with a do block" $ do
        let parser = do
              i <- readInt
              $(string "+")
              j <- readInt
              pure (i + j)
        parser `shouldParseWith` ("2+3", 5)

    describe "(<|>)" $ do
      it "chooses first option on success" $
        (("A" <$ $(string "foo")) <|> ("B" <$ $(string "foo")))
          `shouldParseWith` ("foo", "A")

      it "chooses second option when first fails" $
        (("A" <$ $(string "bar")) <|> ("B" <$ $(string "foo")))
          `shouldParseWith` ("foo", "B")

    describe "branch" $ do
      it "chooses the first branch on success" $
        branch (pure ()) (pure "A") (pure "B") `shouldParseWith` ("", "A")
      it "does not backtrack from first branch" $
        branch (pure ()) empty (pure "B") `shouldParseFail` ""
      it "chooses the second branch on failure" $
        branch empty (pure "A") (pure "B") `shouldParseWith` ("", "B")

    describe "chainl" $ do
      it "parses a chain of numbers" $
        chainl (+) readInt ($(char '+') *> readInt)
          `shouldParseWith` ("1+2+3", 6)

      it "allows the right chain to be empty" $
        chainl (+) readInt ($(char '+') *> readInt)
          `shouldParseWith` ("1", 1)

      it "requires at least the leftmost parser to match" $
        chainl (+) readInt ($(char '+') *> readInt)
          `shouldParseFail` ""

    describe "chainr" $ do
      it "parses a chain of numbers" $
        chainr (+) (readInt <* $(char '+')) readInt
          `shouldParseWith` ("1+2+3", 6)

      it "allows the left chain to be empty" $
        chainr (+) (readInt <* $(char '+')) readInt
          `shouldParseWith` ("1", 1)

      it "requires at least the rightmost parser to match" $
        chainr (+) (readInt <* $(char '+')) readInt
          `shouldParseFail` ""

    describe "many" $ do
      it "parses many chars" $
        many (satisfy isLatinLetter) `shouldParseWith` ("abc", "abc")
      it "accepts empty input" $
        many (satisfy isLatinLetter) `shouldParseWith` ("", "")
      it "is greedy" $
        (many (satisfy isDigit) *> satisfy isDigit) `shouldParseFail` "123"

    describe "many_" $ do
      it "parses many chars" $
        many_ (satisfy isLatinLetter) `shouldParseWith` ("abc", ())
      it "accepts empty input" $
        many_ (satisfy isLatinLetter) `shouldParseWith` ("", ())
      it "is greedy" $
        (many_ (satisfy isDigit) *> satisfy isDigit) `shouldParseFail` "123"

    describe "some" $ do
      it "parses some chars" $
        some (satisfy isLatinLetter) `shouldParseWith` ("abc", "abc")
      it "rejects empty input" $
        some (satisfy isLatinLetter) `shouldParseFail` ""
      it "is greedy" $
        (some (satisfy isDigit) *> satisfy isDigit) `shouldParseFail` "123"

    describe "some_" $ do
      it "parses some chars" $
        some_ (satisfy isLatinLetter) `shouldParseWith` ("abc", ())
      it "rejects empty input" $
        some_ (satisfy isLatinLetter) `shouldParseFail` ""
      it "is greedy" $
        (some_ (satisfy isDigit) *> satisfy isDigit) `shouldParseFail` "123"

    describe "notFollowedBy" $ do
      it "succeeds when it should" $
        readInt `notFollowedBy` $(char '.') `shouldParsePartial` "123+5"
      it "fails when first parser doesn't match" $
        readInt `notFollowedBy` $(char '.') `shouldParseFail` "a"
      it "fails when followed by the wrong thing" $
        readInt `notFollowedBy` $(char '.') `shouldParseFail` "123.0"

    describe "isolate" $ do
      prop "isolate takeRestBs is identity" $ do
        \(bs :: ByteString) ->
          isolate (B.length bs) takeRestBs `shouldParseWith` (bs, bs)
      prop "isolate takeBs length is identity" $ do
        \(bs :: ByteString) ->
          isolate (B.length bs) (takeBs (B.length bs)) `shouldParseWith` (bs, bs)

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

  describe "Getting the rest of the input as a String" $ do
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

  describe "Location & address primitives" $ do
    it "valid lookaheadFromAddr# usage succeeds" $ do
      -- use Int#/Int64# directly because Word8# -> Int# is annoying on old GHCs
      let bs = B.pack [ 0x09, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                      , 0xFF, 0x31, 0x32, 0x33, 0x00, 0xFF]
          p = withAddr# $ \addr# -> withAnyInt64# $ \os# ->
                  lookaheadFromAddr# addr# $ atSkip# os# $ anyCString
      p `shouldParsePartialWith` (bs, "123")

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

--------------------------------------------------------------------------------
-- Some combinators that make it easier to assert the results of a parser.

-- | The parser should parse this string, consuming it entirely, and succeed.
shouldParse :: Show e => Parser e a -> ByteString -> Expectation
p `shouldParse` s = case runParser p s of
  OK _ "" -> pure ()
  OK _ lo -> assertFailure $ "Unexpected leftover: " ++ show lo
  Fail -> assertFailure "Parse failed unexpectedly"
  Err e -> assertFailure $ "Parse threw unexpected error: " ++ show e

-- | The parser should parse this string, possibly with leftovers, and succeed.
shouldParsePartial :: Show e => Parser e a -> ByteString -> Expectation
p `shouldParsePartial` s = case runParser p s of
  OK _ lo -> pure ()
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

-- | The parser should parse this string, possibly with leftovers, and succeed
-- yielding the matching value.
shouldParsePartialWith ::
  (Show a, Eq a, Show e) => Parser e a -> (ByteString, a) -> Expectation
p `shouldParsePartialWith` (s, r) = case runParser p s of
  OK r' lo -> r' `shouldBe` r
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
