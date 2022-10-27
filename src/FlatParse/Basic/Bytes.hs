module FlatParse.Basic.Bytes where

import Language.Haskell.TH
import FlatParse.Basic.Parser
import qualified FlatParse.Common.Assorted as Common
import FlatParse.Basic.Integers ( getWord64OfUnsafe
                                , getWord32OfUnsafe
                                , getWord16OfUnsafe
                                , getWord8OfUnsafe )
import GHC.Exts

-- | Check that the input has at least the given number of bytes.
ensureBytes# :: Int -> Parser e ()
ensureBytes# (I# len) = Parser \fp eob s ->
  case len  <=# minusAddr# eob s of
    1# -> OK# () s
    _  -> Fail#
{-# inline ensureBytes# #-}

-- | Read a sequence of bytes. This is a template function, you can use it as
--   @$(getBytesOf [3, 4, 5])@, for example, and the splice has type @Parser e
--   ()@.
getBytesOf :: [Word] -> Q Exp
getBytesOf bytes = do
  let !len = length bytes
  [| ensureBytes# len >> $(scanBytes# bytes) |]

-- | Template function, creates a @Parser e ()@ which unsafely scans a given
--   sequence of bytes.
scanBytes# :: [Word] -> Q Exp
scanBytes# bytes = do
  let !(leading, w8s) = Common.splitBytes bytes
      !scanw8s        = go w8s where
                         go (w8:[] ) = [| getWord64OfUnsafe w8 |]
                         go (w8:w8s) = [| getWord64OfUnsafe w8 >> $(go w8s) |]
                         go []       = [| pure () |]
  case w8s of
    [] -> go leading
          where
            go (a:b:c:d:[]) = let !w = Common.packBytes [a, b, c, d] in [| getWord32OfUnsafe w |]
            go (a:b:c:d:ws) = let !w = Common.packBytes [a, b, c, d] in [| getWord32OfUnsafe w >> $(go ws) |]
            go (a:b:[])     = let !w = Common.packBytes [a, b]       in [| getWord16OfUnsafe w |]
            go (a:b:ws)     = let !w = Common.packBytes [a, b]       in [| getWord16OfUnsafe w >> $(go ws) |]
            go (a:[])       = [| getWord8OfUnsafe a |]
            go []           = [| pure () |]
    _  -> case leading of

      []              -> scanw8s
      [a]             -> [| getWord8OfUnsafe a >> $scanw8s |]
      ws@[a, b]       -> let !w = Common.packBytes ws in [| getWord16OfUnsafe w >> $scanw8s |]
      ws@[a, b, c, d] -> let !w = Common.packBytes ws in [| getWord32OfUnsafe w >> $scanw8s |]
      ws              -> let !w = Common.packBytes ws
                             !l = length ws
                         in [| scanPartial64# l w >> $scanw8s |]
