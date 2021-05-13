
module ReadInteger where

import qualified Data.Vector.Unboxed as UV
import           FlatParse.Basic as F

readInt     = runParser F.readInt
readInteger = runParser F.readInteger

readInts     = runParser (whitespace *> arrayParser)
checkInts     = runParser (whitespace *> arrayChecker)

arrayParser :: F.Parser e (UV.Vector Int)
arrayParser = openBracket_ *> sepBy int comma_ <* closeBracket_
{-# NOINLINE arrayParser #-}

arrayChecker :: F.Parser e ()
arrayChecker = openBracket_ *> sepBy_ int comma_ <* closeBracket_
{-# NOINLINE arrayChecker #-}

{-# INLINE sepBy #-}
sepBy :: UV.Unbox a => F.Parser e a -> F.Parser e () -> F.Parser e (UV.Vector a)
sepBy elem sep = do
  minitial <- fmap Just elem <|> pure Nothing
  case minitial of
    Nothing -> pure mempty
    Just initial -> do
      rest <- F.many (sep *> elem)
      pure (UV.fromList (initial : rest))

{-# INLINE sepBy_ #-}
sepBy_ :: UV.Unbox a => F.Parser e a -> F.Parser e () -> F.Parser e ()
sepBy_ elem sep = do
  minitial <- fmap Just elem <|> pure Nothing
  case minitial of
    Nothing -> pure mempty
    Just initial -> do
      F.many_ (sep *> elem)

int = F.readInt <* whitespace
openBracket_ = $(char '[') <* whitespace
closeBracket_ = $(char ']') <* whitespace
comma_ = $(char ',') <* whitespace

whitespace :: F.Parser e ()
whitespace =
  F.many_
    $(F.switch
        [|case _ of
            " " -> pure ()
            "\n" -> pure ()|])
