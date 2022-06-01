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

* __Excellent performance__. On microbenchmarks, `flatparse` is around 10 times faster than `attoparsec` or `megaparsec`. On larger examples with heavier use of source positions and spans and/or indentation parsing, the performance difference grows to 20-30 times. Compile times and exectuable sizes are also significantly better with `flatparse` than with `megaparsec` or `attoparsec`. `flatparse` internals make liberal use of unboxed tuples and GHC primops. As a result, pure validators (parsers returning `()`) in `flatparse` are not difficult to implement with zero heap allocation.
* __No incremental parsing__, and __only strict `ByteString`__ is supported as input. However, it can be still useful to convert from `Text`, `String` or other types to `ByteString`, and then use `flatparse` for parsing, since `flatparse` performance usually more than makes up for the conversion costs.
* __Only little-endian 64 bit systems are currently supported as the host machine__. This may change in the future. Getting good performance requires architecture-specific optimizations; I've only considered the most common setting at this point. However, `flatparse` does include specific big-endian parsers for primitive integer types.
* __Support for fast source location handling, indentation parsing and informative error messages__. `flatparse` provides a low-level interface to these. Batteries are _not included_, but it should be possible for users to build custom solutions, which are more sophisticated, but still as fast as possible. In my experience, the included batteries in other libraries often come with major unavoidable overheads, and often we still have to extend existing machinery in order to scale to production features.
* The __backtracking model__ of `flatparse` is different to parsec libraries, and is more close to the [nom](https://github.com/Geal/nom) library in Rust. The idea is that _parser failure_ is distinguished from _parsing error_. The former is used for control flow, and we can backtrack from it. The latter is used for unrecoverable errors, and by default it's propagated to the top. `flatparse` does not track whether parsers have consumed inputs. In my experience, what we really care about is the failure/error distinction, and in `parsec` or `megaparsec` the consumed/non-consumed separation is often muddled and discarded in larger parser implementations. By default, basic `flatparse` parsers can fail but can not throw errors, with the exception of the specifically error-throwing operations. Hence, `flatparse` users have to be mindful about grammar, and explicitly insert errors where it is known that the input can't be valid.

`flatparse` comes in two flavors: [`FlatParse.Basic`][basic] and [`FlatParse.Stateful`][stateful]. Both support a custom error type.

* [`FlatParse.Basic`][basic] only supports the above features. If you don't need indentation
  parsing, this is sufficient.
* [`FlatParse.Stateful`][stateful] additionally supports a built-in `Int` worth of internal state
  and an additional `Int` reader environment. This can support a wide range of indentation parsing
  features. There is a slight overhead in performance and code size compared to `Basic`. However, in
  small parsers and microbenchmarks the difference between `Basic` and `Stateful` is often reduced
  to near zero by GHC and/or LLVM optimization.

## Tutorial

Informative tutorials are work in progress. See [`src/FlatParse/Examples`](src/FlatParse/Examples)
for a lexer/parser example with acceptably good error messages.

## Contribution

Pull requests are welcome. I'm fairly quick to add PR authors as collaborators.

## Some benchmarks

Execution times below. See source code in [bench](bench). Compiled with GHC
8.10.7 `-O2 -fllvm`. Executed on Intel 1165G7 CPU at 28W power draw.

|      benchmark              |  runtime   |
|-----------------------------|-------------
|sexp/fpbasic                 | 1.625 ms   |
|sexp/fpstateful              | 1.815 ms   |
|sexp/attoparsec              | 21.75 ms   |
|sexp/megaparsec              | 33.12 ms   |
|sexp/parsec                  | 98.65 ms   |
|long keyword/fpbasic         | 115.9 μs   |
|long keyword/fpstateful      | 117.7 μs   |
|long keyword/attoparsec      | 2.955 ms   |
|long keyword/megaparsec      | 2.185 ms   |
|long keyword/parsec          | 29.91 ms   |
|numeral csv/fpbasic          | 549.3 μs   |
|numeral csv/fpstateful       | 595.5 μs   |
|numeral csv/attoparsec       | 10.82 ms   |
|numeral csv/megaparsec       | 6.581 ms   |
|numeral csv/parsec           | 39.33 ms   |

Object file sizes for each module containing the `s-exp`, `long keyword` and `numeral csv` benchmarks.

| library    | object file size (bytes) |
| -------    | ------------------------ |
| fpbasic    |  23752                   |
| fpstateful |  25920                   |
| attoparsec |  93584                   |
| megaparsec |  257000                  |
| parsec     |  134296                  |

[basic]: https://hackage.haskell.org/package/flatparse/docs/FlatParse-Basic.html
[stateful]: https://hackage.haskell.org/package/flatparse/docs/FlatParse-Stateful.html
