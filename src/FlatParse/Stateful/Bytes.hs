{- | Bytestring parsers.

Module dependency complications prevent us from placing these in
"FlatParse.Stateful.Base".
-}

module FlatParse.Stateful.Bytes
  ( bytes, bytesUnsafe
  ) where

import FlatParse.Stateful.Parser
import FlatParse.Stateful.Base ( withEnsure )
import FlatParse.Stateful.Integers ( word8Unsafe, word16Unsafe, word32Unsafe, word64Unsafe )
import qualified FlatParse.Common.Assorted as Common
import Language.Haskell.TH
import GHC.Exts

-- | Read a sequence of bytes. This is a template function, you can use it as @$(bytes [3, 4, 5])@,
--   for example, and the splice has type @Parser e ()@. For a non-TH variant see 'byteString'.
bytes :: [Word] -> Q Exp
bytes bs = do
  let !len = length bs
  [| withEnsure len $(bytesUnsafe bs) |]

-- | Template function, creates a @Parser e ()@ which unsafely parses a given
--   sequence of bytes.
--
-- The caller must guarantee that the input has enough bytes.
bytesUnsafe :: [Word] -> Q Exp
bytesUnsafe bytes = do
  let !(leading, w8s) = Common.splitBytes bytes
      !scanw8s        = go w8s where
                         go (w8:[] ) = [| word64Unsafe w8 |]
                         go (w8:w8s) = [| word64Unsafe w8 >> $(go w8s) |]
                         go []       = [| pure () |]
  case w8s of
    [] -> go leading
          where
            go (a:b:c:d:[]) = let !w = Common.packBytes [a, b, c, d] in [| word32Unsafe w |]
            go (a:b:c:d:ws) = let !w = Common.packBytes [a, b, c, d] in [| word32Unsafe w >> $(go ws) |]
            go (a:b:[])     = let !w = Common.packBytes [a, b]       in [| word16Unsafe w |]
            go (a:b:ws)     = let !w = Common.packBytes [a, b]       in [| word16Unsafe w >> $(go ws) |]
            go (a:[])       = [| word8Unsafe a |]
            go []           = [| pure () |]
    _  -> case leading of

      []              -> scanw8s
      [a]             -> [| word8Unsafe a >> $scanw8s |]
      ws@[a, b]       -> let !w = Common.packBytes ws in [| word16Unsafe w >> $scanw8s |]
      ws@[a, b, c, d] -> let !w = Common.packBytes ws in [| word32Unsafe w >> $scanw8s |]
      ws              -> let !w = Common.packBytes ws
                             !l = length ws
                         in [| scanPartial64# l w >> $scanw8s |]

scanPartial64# :: Int -> Word -> ParserT st r e ()
scanPartial64# (I# len) (W# w) = ParserT \fp !r eob s n st ->
  case indexWordOffAddr# s 0# of
    w' -> case uncheckedIShiftL# (8# -# len) 3# of
      sh -> case uncheckedShiftL# w' sh of
        w' -> case uncheckedShiftRL# w' sh of
          w' -> case eqWord# w w' of
            1# -> OK#   st () (plusAddr# s len) n
            _  -> Fail# st
