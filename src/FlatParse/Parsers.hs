module FlatParse.Parsers where

import FlatParse.Basic
import qualified Data.ByteString as B
import Data.Ord ( comparing )
import Data.List ( sortBy )

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
          c <- anyChar
          if '\n' == c then
            go (line + 1) 0 ((i, pos):poss)
          else
            go line (col + 1) ((i, pos):poss)

      sorted :: [(Int, Pos)]
      sorted = sortBy (comparing snd) (zip [0..] poss)

  in case runParser (go 0 0 sorted) str of
       OK res _ -> snd <$> sortBy (comparing fst) res
       _        -> error "invalid position"

-- | Create a `Pos` from a line and column number. Throws an error on out-of-bounds
--   line and column numbers.
mkPos :: B.ByteString -> (Int, Int) -> Pos
mkPos str (line', col') =
  let go line col | line == line' && col == col' = getPos
      go line col = (do
        c <- anyChar
        if c == '\n' then go (line + 1) 0
                     else go line (col + 1)) <|> error "mkPos: invalid position"
  in case runParser (go 0 0) str of
    OK res _ -> res
    _        -> error "impossible"

-- | Break an UTF-8-coded `B.ByteString` to lines. Throws an error on invalid input.
--   This is mostly useful for grabbing specific source lines for displaying error
--   messages.
linesUTF8 :: B.ByteString -> [String]
linesUTF8 str =
  let go = ([] <$ eof) <|> ((:) <$> takeLine <*> go)
  in case runParser go str of
    OK ls _ -> ls
    _       -> error "linesUTF8: invalid input"

-- | Convert an UTF-8-coded `B.ByteString` to a `String`.
unpackUTF8 :: B.ByteString -> String
unpackUTF8 str = case runParser takeRestString str of
  OK a _ -> a
  _      -> error "unpackUTF8: invalid encoding"
