{-# options_ghc -Wno-unused-imports #-}

module Main where

import Data.Primitive.ByteArray
import Gauge

import qualified Data.ByteString.Char8 as B

import qualified Attoparsec
import qualified Megaparsec
import qualified Parsec
--import qualified FPStateful
import qualified FPBasic
import qualified ReadInteger

sexpInp :: B.ByteString
sexpInp =
  B.concat $ "(" : replicate 33333 "(foo (foo (foo ((bar baza)))))" ++ [")"]

longwsInp :: B.ByteString
longwsInp = B.concat $ replicate 55555 "thisisalongkeyword   "

numcsvInp :: B.ByteString
numcsvInp = B.concat ("0" : [B.pack (",  " ++ show n) | n <- [1..100000::Int]])

readIntInp :: B.ByteString
readIntInp = "12345678910"

main :: IO ()
main = defaultMain [
  bgroup "sexp" [
    bench "fpbasic"     $ whnf FPBasic.runSexp    sexpInp,
    --bench "fpstateful"  $ whnf FPStateful.runSexp sexpInp,
    bench "attoparsec"  $ whnf Attoparsec.runSexp sexpInp,
    bench "megaparsec"  $ whnf Megaparsec.runSexp sexpInp,
    bench "parsec"      $ whnf Parsec.runSexp     sexpInp
  ],

  bgroup "long keyword" [
    bench "fpbasic"    $ whnf FPBasic.runLongws    longwsInp,
    --bench "fpstateful" $ whnf FPStateful.runLongws longwsInp,
    bench "attoparsec" $ whnf Attoparsec.runLongws longwsInp,
    bench "megaparsec" $ whnf Megaparsec.runLongws longwsInp,
    bench "parsec"     $ whnf Parsec.runLongws     longwsInp
  ],

  bgroup "numeral csv" [
    bench "fpbasic"    $ whnf FPBasic.runNumcsv    numcsvInp,
    --bench "fpstateful" $ whnf FPStateful.runNumcsv numcsvInp,
    bench "attoparsec" $ whnf Attoparsec.runNumcsv numcsvInp,
    bench "megaparsec" $ whnf Megaparsec.runNumcsv numcsvInp,
    bench "parsec"     $ whnf Parsec.runNumcsv     numcsvInp
  ],

  bgroup "readInt/readInteger" [
    bench "readInt"      $ whnf ReadInteger.readInt     readIntInp,
    bench "readInteger"  $ whnf ReadInteger.readInteger readIntInp
    ]
 ]
