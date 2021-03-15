# flatparse

![CI](https://github.com/AndrasKovacs/flatparse/actions/workflows/haskell.yml/badge.svg)

`flatparse` is a high-performance parsing library, focusing on __programming languages__ and __human-readable data formats__. The "flat" in the name
refers to the `ByteString` parsing input, which has pinned contiguous data, and also to the library internals, which avoids indirections and heap allocations
whenever possible.

## Features and non-features

* __Excellent performance__. On microbenchmarks, `flatparse` is at least 10 times faster than `attoparsec` or `megaparsec`. On larger examples with heavier use of    source positions and spans and/or indentation parsing, the performance difference grows to 20-30 times. Compile times and exectuable sizes are also significantly better with `flatparse` than with `megaparsec` or `attoparsec`. `flatparse` interals make liberal use of unboxed tuples and GHC primops. As a result, pure validators (parsers returning `()`) in `flatparse` are not difficult to implement with zero heap allocation.
* __No incremental parsing__, and __only strict `ByteString`__ is supported as input. However, it can be still useful to convert from `Text`, `String` or other types to `ByteString`, and then use `flatparse` for parsing, since `flatparse` performance usually more than makes up for the conversion costs.
* __Only little-endian 64 bit systems are currently supported__. This may change in the future. Getting good performance requires architecture-specific optimizations; I've only considered the most common setting at this point.
* __Support for fast source location handling, indentation parsing and informative error messages__. `flatparse` provides a low-level interface to these. Batteries are _not included_, but it should be possible for users to build custom solutions, which are more sophisticated, but still as fast as possible. In my experience, the included batteries in other libraries often come with major unavoidable overheads, and often we still have to extend existing machinery in order to scale to production features.
* The __backtracking model__ of `flatparse` is different to parsec libraries, and is more close to the [nom](https://github.com/Geal/nom) library in Rust. The idea is that _parser failure_ is distinguished from _parsing error_. The former is used for control flow, and we can backtrack from it. The latter is used for unrecoverable errors, and by default it's propagated to the top. `flatparse` does not track whether parsers have consumed inputs. In my experience, what we really care about is the failure/error distinction, and in `parsec` or `megaparsec` the consumed/non-consumed separation is often muddled and discarded in larger parser implementations. By default, basic `flatparse` parsers can fail but can not throw errors, with the exception of the specifically error-throwing operations. Hence, `flatparse` users have to be mindful about grammar, and explicitly insert errors where it is known that the input can't be valid.

`flatparse` comes in two flavors: [`FlatParse.Basic`](src/FlatParse/Basic.hs) and [`FlatParse.Stateful`](src/FlatParse/Stateful.hs). Both support a custom error type and a custom reader environment.

* [`FlatParse.Basic`](src/FlatParse/Basic.hs) only supports the above features. If you don't need indentation parsing, this is sufficient.
* [`FlatParse.Stateful`](src/FlatParse/Stateful.hs) additionally supports a built-in `Int` worth of internal state. This can support a wide range of indentation parsing features. There is a slight overhead in performance and code size compared to `Basic`. However, in small parsers and microbenchmarks the difference between `Basic` and `Stateful` is often reduced to near zero by GHC and LLVM optimization. The difference is more marked if we use native code backend instead of LLVM.

The reason for baking a reader into the parsers, is that if we need it, it's convenient, and if we don't, then GHC very reliably optimizes unused environments away. In contrast, GHC optimizes much less reliably if we try to wrap the existing `Reader` from `transformers` around our parsers.

## Tutorial

Informative tutorials are work in progress. See [`src/FlatParse/Examples`](src/FlatParse/Examples) for a lexer/parser example with acceptably good error messages.

### Some benchmarks

Execution times below. See source code in [bench](bench). Compiled with GHC 8.8.4 `-O2 -fllvm`.

|      benchmark              |  runtime   |
|-----------------------------|-------------
| s-exp/fpbasic               |  3.365 ms  |
| s-exp/fpstateful            |  3.421 ms  |
| s-exp/attoparsec            |  42.84 ms  |
| s-exp/megaparsec            |  57.54 ms  |
| s-exp/parsec                |  179.7 ms  |
| long keyword/fpbasic        |  216.4 μs  |
| long keyword/fpstateful     |  299.0 μs  |
| long keyword/attoparsec     |  5.297 ms  |
| long keyword/megaparsec     |  3.646 ms  |
| long keyword/parsec         |  49.18 ms  |
| numeral csv/fpbasic         |  743.5 μs  |
| numeral csv/fpstateful      |  848.5 μs  |
| numeral csv/attoparsec      |  20.64 ms  |
| numeral csv/megaparsec      |  10.12 ms  |
| numeral csv/parsec          |  78.52 ms  |

Object file sizes for each module containing the `s-exp`, `long keyword` and `numeral csv` benchmarks.

| library    | object file size (bytes) |
| -------    | ------------------------ |
| fpbasic    |  26456                   |
| fpstateful |  30008                   |
| attoparsec |  83288                   |
| megaparsec |  188696                  |
| parsec     |  75880                   |
