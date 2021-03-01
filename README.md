# flatparse
Fast parsing from bytestrings.

This is a monadic parser combinator library largely in the tradition of other Haskell parser libraries.

The primary goal of `flatparse` is to be as fast as possible for the primary intended use case, which is parsing programming languages and human-readable data formats. This is achieved at the cost of some features, but which are barely relevant to the intended purpose in my experience. `flatparse` is more minimal
than some other libraries, but it is much faster, and still allows users to build their own custom solutions for fancier features such as indentation parsing
and pretty error reporting.

WIP, documentation and examples to be added.

Observations and principles behind the design.

- Being generic over monads and input streams adds a large overhead on code size, compile times and runtime speed. In real-world usage, reading from strict bytestring covers pretty much all of the programming language parsing tasks, so that's our focus. We do not support streaming and resumption. For non-bytestring inputs, it is still a good idea to first convert/serialize to bytestring, then parse with `flatparse`, instead of trying to parse other representations directly, because of the large speed gains on `flatparse`.
- Continuation-passing style (CPS) is clearly harmful to performance and compile times, at least on modern GHC, but even in the older days of Parsec, when CPS was
first popularized, there was not much if any performance gain either. Instead, we use unboxed tuples in a monomorphic state monad, where the state is simply a machine address pointing inside a pinned bytestring.
- We focus on UTF-8 inputs currently, with some optimizations for ASCII. More support for other encodings and raw byte input could be added in the future. However, UTF-8 and ASCII should already cover much of language parsing tasks.
- Without more serious optimization, lexical analysis is very inefficient in naive monadic parsing. To address this, we use Template Haskell for literal parsing and for branching on statically known choices of keywords. For example, reading a concrete keyword in `flatparse` is UTF-8 decoded at compile time, and also vectorized to multi-byte reads. Multiple keyword matching is compiled to trie-shaped reads and comparisons.
