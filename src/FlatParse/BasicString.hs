module FlatParse.BasicString
  (
    runParserS

  , takeLine
  , traceLine
  , takeRestString
  , traceRestString

  , packUTF8
  , unpackUTF8

  , validPos
  , posLineCols
  , unsafeSpanToByteString
  , mkPos
  , lines

  ) where

import Prelude hiding ( getChar, lines )

import FlatParse.Basic

import Data.Ord ( comparing )
import Data.List ( sortBy )
import FlatParse.Common.Assorted ( packUTF8 )

import qualified Data.ByteString as B

-- | Run a parser on a `String` input. Reminder: @OverloadedStrings@ for `B.ByteString` does not
--   yield a valid UTF-8 encoding! For non-ASCII `B.ByteString` literal input, use `runParserS` or
--   `packUTF8` for testing.
runParserS :: Parser e a -> String -> Result e a
runParserS pa s = runParser pa (packUTF8 s)

-- | Create a `Pos` from a line and column number. Throws an error on out-of-bounds
--   line and column numbers.
mkPos :: B.ByteString -> (Int, Int) -> Pos
mkPos str (line', col') =
  let go line col | line == line' && col == col' = getPos
      go line col = (do
        c <- getChar
        if c == '\n' then go (line + 1) 0
                     else go line (col + 1)) <|> error "mkPos: invalid position"
  in case runParser (go 0 0) str of
    OK res _ -> res
    _        -> error "impossible"

-- | Break an UTF-8-coded `B.ByteString` to lines. Throws an error on invalid
--   input. This is mostly useful for grabbing specific source lines for
--   displaying error messages.
lines :: B.ByteString -> [String]
lines str =
  let go = ([] <$ eof) <|> ((:) <$> takeLine <*> go)
  in case runParser go str of
    OK ls _ -> ls
    _       -> error "linesUTF8: invalid input"


-- | Check whether a `Pos` points into a `B.ByteString`.
validPos :: B.ByteString -> Pos -> Bool
validPos str pos =
  let go = do
        start <- getPos
        pure (start <= pos && pos <= endPos)
  in case runParser go str of
    OK b _ -> b
    _      -> error "impossible"
{-# inline validPos #-}

-- | Compute corresponding line and column numbers for each `Pos` in a list. Throw an error
--   on invalid positions. Note: computing lines and columns may traverse the `B.ByteString`,
--   but it traverses it only once regardless of the length of the position list.
posLineCols :: B.ByteString -> [Pos] -> [(Int, Int)]
posLineCols str poss =
  let go !line !col [] = pure []
      go line col ((i, pos):poss) = do
        p <- getPos
        if pos == p then
          ((i, (line, col)):) <$> go line col poss
        else do
          c <- getChar
          if '\n' == c then
            go (line + 1) 0 ((i, pos):poss)
          else
            go line (col + 1) ((i, pos):poss)

      sorted :: [(Int, Pos)]
      sorted = sortBy (comparing snd) (zip [0..] poss)

  in case runParser (go 0 0 sorted) str of
       OK res _ -> snd <$> sortBy (comparing fst) res
       _        -> error "invalid position"

-- | Create a `B.ByteString` from a `Span`. The result is invalid if the `Span` points
--   outside the current buffer, or if the `Span` start is greater than the end position.
unsafeSpanToByteString :: Span -> Parser e B.ByteString
unsafeSpanToByteString (Span l r) =
  lookahead (setPos l >> byteStringOf (setPos r))
{-# inline unsafeSpanToByteString #-}

--------------------------------------------------------------------------------

-- | Parse the rest of the current line as a `String`. Assumes UTF-8 encoding,
--   throws an error if the encoding is invalid.
takeLine :: Parser e String
takeLine = branch eof (pure "") do
  c <- getChar
  case c of
    '\n' -> pure ""
    _    -> (c:) <$> takeLine

-- | Parse the rest of the current line as a `String`, but restore the parsing state.
--   Assumes UTF-8 encoding. This can be used for debugging.
traceLine :: Parser e String
traceLine = lookahead takeLine

--------------------------------------------------------------------------------

-- | Convert an UTF-8-coded `B.ByteString` to a `String`.
unpackUTF8 :: B.ByteString -> String
unpackUTF8 str = case runParser takeRestString str of
  OK a _ -> a
  _      -> error "unpackUTF8: invalid encoding"

-- | Take the rest of the input as a `String`. Assumes UTF-8 encoding.
takeRestString :: Parser e String
takeRestString = branch eof (pure "") do
  c <- getChar
  cs <- takeRestString
  pure (c:cs)

-- | Get the rest of the input as a `String`, but restore the parsing state. Assumes UTF-8 encoding.
--   This can be used for debugging.
traceRestString :: Parser e String
traceRestString = lookahead takeRestString
