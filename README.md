# flatparse

`flatparse` is a high-performance parsing library, focusing on __programming languages__ and __human-readable data formats__. The "flat" in the name
refers to the `ByteString` parsing input, which has pinned contiguous data, and also to the library internals, which avoids indirections and heap allocations
whenever possible.

## Features and non-features

* __Excellent performance__. On microbenchmarks, `flatparse` is at least 10 times faster than `attoparsec` or `megaparsec`. On larger examples with heavier use of    source positions and spans and/or indentation parsing, the performance difference grows to 20-30 times. Pure validators (parsers returning `()`) in `flatparse` are not difficult to implement with zero heap allocation. Compile times and exectuable sizes are also significantly better with `flatparse` than with `megaparsec` or `attoparsec`. 
* `flatparse` interals make liberal use of unboxed tuples and GHC primops. As a result, pure validators (parsers returning `()`) in `flatparse` are not difficult to implement with zero heap allocation. 
* __No incremental parsing__, and __only strict `ByteString`__ is supported as input. However, it can be still useful to convert from `Text`, `String` or other types to `ByteString`, and then use `flatparse` for parsing, since `flatparse` performance usually more than makes up for the conversion costs.
* __Only little-endian 64 bit systems are currently supported__. This may change in the future. Getting good performance requires architecture-specific optimizations; I've only considered the most common setting at this point. 
* __Support for fast source location handling, indentation parsing and informative error messages__. `flatparse` provides a low-level interface to these. Batteries are _not included_, but it should be possible for users to build custom solutions, which are more sophisticated, but still as fast as possible. In my experience, the included batteries in other libraries often come with major unavoidable overheads, and often we still have to extend existing machinery in order to scale to production features.

`flatparse` comes in two flavors: `FlatParse.Basic` and `FlatParse.Stateful`. Both support a custom error type and a custom reader environment. 

* `FlatParse.Basic` only supports the above features. If you don't need indentation parsing, this is sufficient.
* `FlatParse.Stateful` additionally supports a built-in `Int` worth of internal state. This can support a wide range of indentation parsing features. There is a slight overhead in performance and code size compared to `Basic`. However, in small parsers and microbenchmarks the difference between `Basic` and `Stateful` is often reduced to near zero by GHC and LLVM optimization. The difference is more marked if we use native code backend instead of LLVM.

## Tutorial

TODO
  
### Some benchmarks

Execution times below. See source code in [bench](bench). Compiled with GHC 8.8.4 `-O2 -fllvm`. 

|      benchmark              |  runtime   | 
|-----------------------------|-------------
| s-exp/fpbasic               |  3.597 ms  |
| s-exp/fpstateful            |  3.630 ms  |
| s-exp/attoparsec            |  44.70 ms  |
| s-exp/megaparsec            |  58.47 ms  |
| s-exp/parsec                |  193.3 ms  |
| long keyword/fpbasic        |  327.4 μs  |
| long keyword/fpstateful     |  325.5 μs  |
| long keyword/attoparsec     |  6.009 ms  |
| long keyword/megaparsec     |  3.574 ms  |
| long keyword/parsec         |  49.21 ms  |
| numeral csv/fpbasic         |  881.0 μs  |
| numeral csv/fpstateful      |  854.7 μs  |
| numeral csv/attoparsec      |  21.51 ms  |
| numeral csv/megaparsec      |  10.36 ms  |
| numeral csv/parsec          |  80.64 ms  |

Object file sizes for each module containing the `s-exp`, `long keyword` and `numeral csv` benchmarks.

| library    | object file size (bytes) |
| -------    | ------------------------ |
| fpbasic    |  25392                   |
| fpstateful |  30056                   |
| attoparsec |  83288                   |
| megaparsec |  188696                  |
| parsec     |  75880                   |
