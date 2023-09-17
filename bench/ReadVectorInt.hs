{-# language ScopedTypeVariables, Strict #-}
{-# options_ghc -ddump-simpl -dsuppress-all -dno-suppress-type-signatures -ddump-to-file #-}

-- see:https://gist.github.com/chrisdone/8551675bb99a0d66cf075fdcb1e6b757

module ReadVectorInt (readInts, readInts', checkInts) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import FlatParse.Basic as F
import GHC.Exts

import qualified Data.ByteString.Char8 as B

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

whitespace :: F.ParserT s e ()
whitespace =
  F.skipMany
    $(F.switch
        [|case _ of
            " " -> pure ()
            "\n" -> pure ()|])

int :: F.ParserT s e Int
int = F.anyAsciiDecimalInt <* whitespace

openBracket_ :: F.ParserT s e ()
openBracket_ = $(char '[') <* whitespace

closeBracket_ :: F.ParserT s e ()
closeBracket_ = $(char ']') <* whitespace

comma_ :: F.ParserT s e ()
comma_ = $(char ',') <* whitespace

{-# inline manyInVec #-}
manyInVec :: forall e s a. UV.Unbox a => F.ParserT s e a -> F.ParserT s e (UV.Vector a)
manyInVec p = let

  go :: forall s'. Int -> ParserST s' e (MUV.STVector s' a)
  go i = F.withOption (unsafeCoerce# p)
     (\a -> do
         vec <- go (i + 1)
         F.liftST $ MUV.unsafeWrite vec i a
         pure vec)
     (F.liftST $ MUV.unsafeNew i)

  go2 :: forall s'. ParserST s' e (UV.Vector a)
  go2 = do
    vec <- go 0
    F.liftST $ UV.unsafeFreeze vec

  in unsafeCoerce# go2

{-# INLINE sepBy' #-}
sepBy' :: UV.Unbox a => F.Parser e a -> F.Parser e () -> F.Parser e (UV.Vector a)
sepBy' elem sep =
  F.withOption elem
    (\initial -> do
      rest <- manyInVec (sep *> elem)
      pure $! UV.cons initial rest)
    (pure mempty)

arrayParser' :: F.Parser e (UV.Vector Int)
arrayParser' = openBracket_ *> sepBy' int comma_ <* closeBracket_
{-# NOINLINE arrayParser' #-}

readInts' :: B.ByteString -> Result () (UV.Vector Int)
readInts' = runParser (whitespace *> arrayParser')
