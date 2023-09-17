{-# language ScopedTypeVariables, Strict #-}
{-# options_ghc -ddump-simpl -dsuppress-all -dno-suppress-type-signatures -ddump-to-file #-}

module ReadVectorInt (readInts, checkInts) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import FlatParse.Basic as F
import Control.Monad.ST

readInts     = runParser (whitespace *> arrayParser)
checkInts    = runParser (whitespace *> arrayChecker)

arrayParser :: F.Parser e (UV.Vector Int)
arrayParser = openBracket_ *> sepBy int comma_ <* closeBracket_
{-# NOINLINE arrayParser #-}

arrayChecker :: F.Parser e ()
arrayChecker = openBracket_ *> sepBy_ int comma_ <* closeBracket_
{-# NOINLINE arrayChecker #-}

{-# INLINE sepBy #-}
sepBy :: UV.Unbox a => F.Parser e a -> F.Parser e () -> F.Parser e (UV.Vector a)
sepBy elem sep =
  F.withOption elem
    (\initial -> do
      rest <- F.many (sep *> elem)
      pure (UV.fromList (initial : rest)))
    (pure mempty)

{-# INLINE sepBy_ #-}
sepBy_ :: UV.Unbox a => F.Parser e a -> F.Parser e () -> F.Parser e ()
sepBy_ elem sep = do
  F.withOption elem
    (\initial -> F.skipMany (sep *> elem))
    (pure ())

int = F.anyAsciiDecimalInt <* whitespace
openBracket_ = $(char '[') <* whitespace
closeBracket_ = $(char ']') <* whitespace
comma_ = $(char ',') <* whitespace

whitespace :: F.Parser e ()
whitespace =
  F.skipMany
    $(F.switch
        [|case _ of
            " " -> pure ()
            "\n" -> pure ()|])


-- vecMany :: forall e a s. UV.Unbox a => F.ParserST s e a -> F.ParserST s e (MUV.STVector s a)
-- vecMany p = go 0 where
--   go :: Int -> F.Parser e (UV.Vector a)
--   go ix = F.withOption p
--     (\a -> _)
--     (do
