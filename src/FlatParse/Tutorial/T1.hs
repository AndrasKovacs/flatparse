{-# language TemplateHaskell, StrictData, OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}

module FlatParse.Tutorial.T1 where

import FlatParse.Basic hiding (Parser)
import qualified FlatParse.Basic as FP
import Data.ByteString (ByteString)
import Language.Haskell.TH (ppr, runQ, Q, Exp)

{-
A major difference in flatparse compared to *parsec, is that we use Template Haskell for basic token
reading.

Hence, we should pretty much always turn on {-# language TemplateHaskell #-} when using this library.
See the docs for more info on TH:

    https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/template_haskell.html

First, let's define a parser type synonym:
-}

type Parser a = FP.Parser () String a

{-
This means that we have () as reader environment, and we can throw String errors.

Let's define a parser for a concrete string:
-}

pFoo :: Parser ()
pFoo = $(string "foo")

{-
string is a TH function which unfolds to a parser with type Parser r e (). Why do we need TH? The
reason is that trying to parse "foo" character by character, or by comparing the input to some
in-memory string created from "foo", is hopelessly slow compared to what's possible.

In flatparse, $(string "foo") performs UTF-8 decoding to a concrete byte sequence, then vectorizes
its reading to some combination of 1-8 byte reads. In this case, "foo" consists of 3 bytes, so we
read it by first getting 2 bytes, then 1 byte, by two machine intructions.

If we have something with 8 bytes, like "fooobaar", we can read that with a single 64-bit read
instruction.

If you want to look at the generated code for a TH definition, you can use ppr and runQ from
Language.Haskell.TH like below:

    ppr <$> runQ (string "foo")

this prints:

    FlatParse.Basic.ensureBytes# 3 GHC.Base.>>
    (FlatParse.Basic.scan16# 28518 GHC.Base.>> FlatParse.Basic.scan8# 111)

Cleaned up:

    ensureBytes# 3 >> scan16# 28518 >> scan8# 111

So what happens here is that we first check that the input has at least 3 bytes, then
perform the reading.
-}

{-
Now let's run this. We can use either

    runParser :: Parser r e a -> r -> ByteString -> Result e a

or

    runParserS :: Parser r e a -> r -> String -> Result e a

The latter is intended for quick testing, so that we don't have to turn on OverloadedStrings to get
ByteString literals.

Result is defined as

    data Result e a = OK a !ByteString | Fail | Err !e

We will later look at the disctinction between Fail and Err, and how it relates to backtracking.
-}

test1 = runParserS pFoo () "foo" -- OK () ""

{-
We get the remaining input as the last field of OK:
-}

test2 = runParserS pFoo () "foobar" -- OK () "bar"

{-
Let's check out a failing case:
-}

test3 = runParserS pFoo () "bar" -- Fail

{-
This returns a plain Fail and no other information. We can give a more informative error:
-}

pFoo2 :: Parser ()
pFoo2 = pFoo `cut` "expected a \"foo\""

test4 = runParserS pFoo2 () "bar" -- Err "expected a \"foo\""

{-
The error handling and backtracking mechanism of flatparse is modeled after the nom library in Rust:

    https://github.com/Geal/nom

This means that *failures* and *errors* are distinct:

  - Failures are used for control flow purposes, and we can backtrack from them by default.
  - Errors are unrecoverable by default.

When we really want a "foo" to be there, or else the input is necessarily invalid, we can
throw an error in several ways.

    cut     :: Parser r e a -> e -> Parser r e a
    cutting :: Parser r e a -> e -> (e -> e) -> Parser r e a
    err     :: e -> Parser r e a

  - "cut" converts a failure to an error: if the argument parser returns a failure,
    "cut" throws an error. If the argument parser return with an error, then that
    error is propagated outwards.

  - "cutting p e f" allow us to combine inner and outer errors using the "f :: e -> e"
    function. If "p" throws an error, then we apply "f" to it, and re-throw that.

  - "err" simply throws an error.

A key difference between Err and Fail is that when we choose between parsers, we
backtrack from Fail but not from Err. A simple example:
-}

pFoobar :: Parser ()
pFoobar = ($(string "foo") >> $(string "bar")) <|> $(string "foo")

test5 = runParserS pFoobar () "foo" -- OK () ""

{-
Above, we first try to parser "foo", which succeeds, then "bar", which fails, then we backtrack and
try "foo" from the right side of (<|>). Unlike in parsec, (<|>) was not bothered by the fact
that the left side failed after having consumed input.
-}

pFoobar2 :: Parser ()
pFoobar2 = ($(string "foo") >> ($(string "bar") `cut` "there must be \"bar\""))
       <|> $(string "foo")

test6 = runParserS pFoobar2 () "foo" -- Err "there must be \"bar\""

{-
The "cut" though propagated to the top.
-}

{-
  Let's look at slightly bigger example. This is the s-expression validator
  from the benchmarks.
-}

ws :: Parser ()
ws = many_ $(switch [| case _ of
  " "  -> pure ()
  "\n" -> pure () |])

{-
This is the whitespace parser. The "switch" expression is again a TH function, which compiles to
efficient matching. The "_" in "case _ of" is mandatory, we just reuse the native "case" for
convenient syntax. Switching has longest match semantics, so the order of clauses does not matter.

If no case matches, a "switch" fails.
-}

open :: Parser ()
open = $(char '(') >> ws

close :: Parser ()
close = $(char ')') >> ws

{-
At this point we may ask: why not define a different version of "char" which
consumes whitespace? Let's try:
-}
char' :: Char -> Q Exp
char' c = [| $(char c) >> ws |]

{-
Unfortunately, GHC staging restriction mandates that any TH function used in a module,
must be defined in an imported module. So the following is *not* allowed:
-}
-- open' c = $(char' c)

{-
Hence, it generally makes sense in flatparse to define a "lexer" module, containing
whitespace parsing and token parsers, and a "parser" module, containing everything else.
This way we can define TH shorthands in the "lexer" module.
-}

ident :: Parser ()
ident = some_ (satisfyASCII isLatinLetter) >> ws

sexp :: Parser ()
sexp = branch open (some_ sexp >> close) ident

{-
"branch" is like an if-then-else for parsers. If the first parser succeds, we continue
with the second, if it fails, with the third.
-}

src :: Parser ()
src = sexp >> eof

validSexp :: ByteString -> Bool
validSexp s = case runParser src () s of
  OK{} -> True
  _    -> False

{-
Examples.
-}

test7 = validSexp "(foo bar baz)" -- True
test8 = validSexp "(foo bar baz"  -- False
test9 = validSexp "(f (x y z) (foo bar (baz k)))" -- True
