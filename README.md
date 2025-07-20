
<!-- Intro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# **fptk** — functional toolkit for hy-lang

**fptk** is curated list of math and FP-related modules/classes/functions/macroses,
that are supposed to be imported into main scope altogether to always have them at your fingertips.
> Having high amount of functions available in main context is inspired by Wolfram Language,
> in which all (yes, ALL) standard functions are always in the main context.

Intended usage of **fptk** is:
```hy
(import fptk *)     ; import of modules, functions and classes
(require fptk *)    ; import of macros
```

---

Table of Contents:
- [Topics that fptk covers](#Topics-that-fptk-covers)
- [Documentation](#Documentation)
- [Installation](#Installation)

<!-- __________________________________________________________________________/ }}}1 -->

<!-- Topics ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Topics that fptk covers

**fptk** aims to unite and bring into main scope:
- [x] Full modules that are math and FP-relevant (math, operator, itertools, ...)
- [x] Buffed getters (first, second, last, rest, ...)
- [x] 1-based index variants of basic getters (I can't stand slicing with 0-based index)
- [x] [lenses](https://github.com/ingolemo/python-lenses) for manipulating deeply nested immutable data
- [x] APL-like functions for mapping, filtering, partitioning, etc. (source libs: funcy, itertools, etc.)
- [x] Functional composition/piping/currying (source libs: hyrule, funcy, itertools, etc.)
- [x] Math, random generators, logic/checks
- [x] Strings manipulation and regexes 
- [x] Basic types like Any, Optional, etc. (source libs: typing, [pydantic](https://github.com/pydantic/pydantic))
- [x] Utils for benchmarking and debug

Currently under development:
- [ ] mini-DSL for SQL-style queries into collections
- [ ] Strict type checking (source lib: pydantic)
- [ ] Immutable structures (source lib: under consideration)
- [ ] Monadic machinery (source libs: [returns](https://github.com/dry-python/returns))

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Documentation ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## Documentation

1. [Functions and modules](https://github.com/rmnavr/fptk/blob/main/docs/functions.md) — auto-generated table with concise description for each entity
2. [Basic macros](https://github.com/rmnavr/fptk/blob/main/docs/macros.md) — every fptk macro except lens-related macros
3. [Lens related macros](https://github.com/rmnavr/fptk/blob/main/docs/lens.md) — macros that simplify lens definition/application/composition

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Installation ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Installation

```
pip install git+https://github.com/rmnavr/fptk.git@0.2.0
```

<!-- __________________________________________________________________________/ }}}1 -->

