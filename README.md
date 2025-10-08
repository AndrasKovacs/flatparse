# flatparse

[![Hackage](https://img.shields.io/hackage/v/flatparse.svg)](https://hackage.haskell.org/package/flatparse)
![CI](https://github.com/AndrasKovacs/flatparse/actions/workflows/haskell.yml/badge.svg)

`flatparse` is a high-performance parsing library, supporting parsing for __programming languages__, __human-readable data__ and __machine-readable data__. The "flat" in the name refers to the `ByteString` parsing input, which has pinned contiguous data, and also to the library internals, which avoids indirections and heap allocations whenever possible. `flatparse` is generally __lower-level__ than `parsec`-style libraries, but it is possible to build higher-level features (such as source spans, hints, indentation parsing) on top of it, without making any compromises in performance.

### LLVM

It is advised to build with [`-fllvm`
option](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/codegens.html#llvm-code-generator-fllvm)
when using this package, since that can result in significant speedups (20-40%
from what I've seen). Additionally, you can enable `-fllvm` for `flatparse`
specifically by enabling the `llvm` package flag. However, this has minor
impact, since almost all parser code will be typically inlined into modules
outside `flatparse`, and compiled there.

## Features and non-features

* __Excellent performance__. On microbenchmarks, `flatparse` is 2-10 times faster than `attoparsec` or `megaparsec`. On examples with heavier use of source positions and spans and/or indentation parsing, you can expect a bigger gap between `megaparsec` and `flatparse`. Compile times and executable sizes are also significantly better with `flatparse` than with `megaparsec` or `attoparsec`. `flatparse` internals make liberal use of unboxed tuples and GHC primops. As a result, pure validators (parsers returning `()`) in `flatparse` are not difficult to implement with zero heap allocation.
* __No incremental parsing__, and __only strict `ByteString`__ is supported as input. However, it can be still useful to convert from `Text`, `String` or other types to `ByteString`, and then use `flatparse` for parsing, since `flatparse` performance usually more than makes up for the conversion costs.
* __Only little-endian systems are currently supported as the host machine__. This may change in the future. However, `flatparse` does include primitive integer parsers with specific endianness.
* __Support for fast source location handling, indentation parsing and informative error messages__. `flatparse` provides a low-level interface to these. Batteries are _not included_, but it should be possible for users to build custom solutions, which are more sophisticated, but still as fast as possible. In my experience, the included batteries in other libraries often come with major unavoidable overheads, and often we still have to extend existing machinery in order to scale to production features.
* The __backtracking model__ of `flatparse` is different to parsec libraries, and is more close to the [nom](https://github.com/Geal/nom) library in Rust. The idea is that _parser failure_ is distinguished from _parsing error_. The former is used for control flow, and we can backtrack from it. The latter is used for unrecoverable errors, and by default it's propagated to the top. `flatparse` does not track whether parsers have consumed inputs. In my experience, what we really care about is the failure/error distinction, and in `parsec` or `megaparsec` the consumed/non-consumed separation is often muddled and discarded in larger parser implementations. By default, basic `flatparse` parsers can fail but can not throw errors, with the exception of the specifically error-throwing operations. Hence, `flatparse` users have to be mindful about grammar, and explicitly insert errors where it is known that the input can't be valid.

`flatparse` comes in two flavors: [`FlatParse.Basic`][basic] and [`FlatParse.Stateful`][stateful]. Both support a custom error type. Also, both come in three modes, where we can respectively run `IO` actions, `ST` actions, or no side effects. The modes are selected by a state token type parameter on the parser types.

* [`FlatParse.Basic`][basic] only supports the above features. If you don't need
  indentation parsing, this is sufficient.
* [`FlatParse.Stateful`][stateful] additionally supports a built-in `Int` worth
  of internal state and an additional custom reader environment. This can
  support a wide range of indentation parsing features. There is a moderate
  overhead in performance and code size compared to `Basic`. In microbenchmarks
  and small parsers, the performance difference between `Basic` and `Stateful`
  is more up to the whims of GHC and LLVM, and is a bit more "random".

## Tutorial

Informative tutorials are work in progress. See [`src/FlatParse/Examples`](src/FlatParse/Examples)
for a lexer/parser example with acceptably good error messages.

## Contribution

Pull requests are welcome. I'm fairly quick to add PR authors as collaborators.

## Some benchmarks

Execution times below. See source code in [bench](bench). Compiled with GHC 9.10.2 `-O2 -fllvm` with
`flatparse-0.5.3.1`. Executed on AMD 9800X3D CPU. Uses `lts-24.7` Stackage snapshot for the involved
packages.

|      benchmark              |  runtime   |
|-----------------------------|-------------
|sexp/fpbasic | 1.80 ms|
|sexp/fpstateful | 1.25 ms|
|sexp/attoparsec | 10.2 ms|
|sexp/megaparsec | 6.92 ms|
|sexp/parsec | 39.9 ms|
|long keyword/fpbasic | 0.054 ms |
|long keyword/fpstateful | 0.062 ms |
|long keyword/attoparsec | 0.308 ms |
|long keyword/megaparsec | 0.687 ms |
|long keyword/parsec | 3.50 ms |
|numeral csv/fpbasic | 0.540 ms |
|numeral csv/fpstateful | 0.504 ms |
|numeral csv/attoparsec | 3.17 ms |
|numeral csv/megaparsec | 1.09 ms |
|numeral csv/parsec | 13.8 ms |
|lambda term/fpbasic | 1.52 ms|
|lambda term/fpstateful | 1.56 ms|
|lambda term/attoparsec | 4.94 ms|
|lambda term/megaparsec | 5.35 ms|
|lambda term/parsec | 17.7 ms|

Object file sizes for each module containing the `s-exp`, `long keyword`, `numeral csv` and `lambda term` benchmarks.

| library    | object file size (bytes) |
| -------    | ------------------------ |
| fpbasic    |  71088                   |
| fpstateful |  73576                   |
| attoparsec |  242816                  |
| megaparsec |  402984                  |
| parsec     |  329008                  |

[basic]: https://hackage.haskell.org/package/flatparse/docs/FlatParse-Basic.html
[stateful]: https://hackage.haskell.org/package/flatparse/docs/FlatParse-Stateful.html
