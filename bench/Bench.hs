{-# options_ghc -Wno-unused-imports #-}

module Main where

import Data.Primitive.ByteArray
import Test.Tasty.Bench

import qualified Data.ByteString.Char8 as B

import qualified Attoparsec
import qualified Megaparsec
import qualified Parsec
import qualified FPStateful
import qualified FPBasic
import qualified ReadInteger

import qualified Data.ByteString.UTF8
import FlatParse.Common.Assorted (strToUtf8)
import qualified FlatParse.Basic

import Common

sexpInp :: B.ByteString
sexpInp =
  B.concat $ "(" : replicate 33333 "(foo (foo (foo ((bar baza)))))" ++ [")"]

longwsInp :: B.ByteString
longwsInp = B.concat $ replicate 55555 "thisisalongkeyword   "

numcsvInp :: B.ByteString
numcsvInp = B.concat ("0" : [B.pack (",  " ++ show n) | n <- [1..100000::Int]])

readIntInp :: B.ByteString
readIntInp = "12345678910"

longString :: String
longString =
  concat $ "(" : replicate 33333 "(foo (foo (foo ((bar baza)))))" ++ [")"]

tmInp :: B.ByteString
tmInp = B.pack (unlines (do
  x <- [0..3000::Int]
  pure ("let x" ++ show x ++ " = fun f. fun g. fun x. fun y. f (f (f ((g x y + g x y) * g x y * g x y * 13500)));")
  ++ ["x1000"]))


main :: IO ()
main = defaultMain [
{-
  bgroup "String -> UTF-8 ByteString" [
    bench "utf8-string" $ whnf Data.ByteString.UTF8.toString sexpInp,
    bench "fp" $ whnf FlatParse.Common.Assorted.utf8ToStr sexpInp
  ],
  bgroup "UTF-8 ByteString -> String" [
    bench "utf8-string" $ whnf Data.ByteString.UTF8.fromString longString,
    bench "fp" $ whnf FlatParse.Common.Assorted.strToUtf8 longString
  ]
 ]
-}
  bgroup "sexp" [
    bench "fpbasic"     $ whnf FPBasic.runSexp    sexpInp,
    bench "fpstateful"  $ whnf FPStateful.runSexp sexpInp,
    bench "attoparsec"  $ whnf Attoparsec.runSexp sexpInp,
    bench "megaparsec"  $ whnf Megaparsec.runSexp sexpInp,
    bench "parsec"      $ whnf Parsec.runSexp     sexpInp
  ],

  bgroup "long keyword" [
    bench "fpbasic"    $ whnf FPBasic.runLongws    longwsInp,
    bench "fpstateful" $ whnf FPStateful.runLongws longwsInp,
    bench "attoparsec" $ whnf Attoparsec.runLongws longwsInp,
    bench "megaparsec" $ whnf Megaparsec.runLongws longwsInp,
    bench "parsec"     $ whnf Parsec.runLongws     longwsInp
  ],

  bgroup "numeral csv" [
    bench "fpbasic"    $ whnf FPBasic.runNumcsv    numcsvInp,
    bench "fpstateful" $ whnf FPStateful.runNumcsv numcsvInp,
    bench "attoparsec" $ whnf Attoparsec.runNumcsv numcsvInp,
    bench "megaparsec" $ whnf Megaparsec.runNumcsv numcsvInp,
    bench "parsec"     $ whnf Parsec.runNumcsv     numcsvInp
  ],

  bgroup "lambda term" [
    bench "fpbasic"    $ whnf FPBasic.runTm    tmInp,
    bench "fpstateful" $ whnf FPStateful.runTm tmInp,
    bench "attoparsec" $ whnf Attoparsec.runTm tmInp,
    bench "megaparsec" $ whnf Megaparsec.runTm tmInp,
    bench "parsec"     $ whnf Parsec.runTm tmInp
  ],

  bgroup "readInt/readInteger" [
    bench "readInt"      $ whnf ReadInteger.readInt     readIntInp,
    bench "readInteger"  $ whnf ReadInteger.readInteger readIntInp
    ]
 ]
