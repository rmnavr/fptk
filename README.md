
<!-- Intro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# **fptk** — functional toolkit for hy-lang

fptk is general purpose functional programming language extension for hy-lang.

It is implemented as a curated list of FP-related types/classes, functions and macroses,
that are supposed to be imported into main scope altogether to always have them at your fingertips.
> Having high amount of functions available in main context is inspired by Wolfram Language,
> in which all (yes, ALL) standard functions are always in the main context.

Intended usage of fptk in hy is:
```hy
(import fptk *)     ; import modules, functions and types/classes
(require fptk *)    ; import macros
```

<!-- __________________________________________________________________________/ }}}1 -->

<!-- Topics ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Topics that fptk covers

Main parts of FPTK are:

Basic operations in FP-style:
- Math, logic and checks (most checks in fptk end with "Q": like `intQ`, `zerolenQ`, etc.)
- String manipulation and regexes
- Basic IO (functions like `read_file` and `file_existsQ`)
- Basic benchmarking and debugging

APL-like functions (heavily inspired by [funcy](https://github.com/Suor/funcy/) lib):
> fptk follows funcy approach of providing both generator
> and list version of most functions (like `map` and `lmap`).
> Also, most functions do not mutate data.
- Buffed getters (first, second, last, rest, ...)
- Functional composition, piping, partial application and currying
- Mapping, filtering and partitioning of sequences

Typing:
- Reimport of basic types like `Any`, `Optional`, etc.
- Dynamic type checking to an extent that is offered by [pydantic](https://github.com/pydantic/pydantic)

Other quirky things:
- 1-based index variants of basic getters (don't worry, fptk does not force using them)
- macros for lenses ([lenses](https://github.com/ingolemo/python-lenses) is Haskell-ish approach for manipulating deeply nested immutable data)
- Generic Result type (inspired by Haskell's Either monad) that can be type-checked by pydantic

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Dependencies ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Dependencies

Tested with:
* [hy](https://github.com/hylang/hy) 1.0.0
* [hyrule](https://github.com/hylang/hyrule) 1.0.0
* [funcy](https://github.com/Suor/funcy/) 2.0
* [lenses](https://github.com/ingolemo/python-lenses) 1.2 
* [pydantic](https://github.com/pydantic/pydantic) 2.0

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Documentation ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## Documentation

1. [Functions and modules](https://github.com/rmnavr/fptk/blob/main/docs/functions.md) — auto-generated table with concise description for each entity
2. [Basic macros](https://github.com/rmnavr/fptk/blob/main/docs/macros.md) — detailed description of every fptk macro (except lens-related macros)
3. [Lens related macros](https://github.com/rmnavr/fptk/blob/main/docs/lens.md) — macros that simplify lens definition/application/composition
4. [Result type](https://github.com/rmnavr/fptk/blob/main/docs/resultM.md) — API of fptk implementation of Result type

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Project status ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Project status

As of October 2025:
- [x] full documentation is done
- [x] testing suite is written
- [ ] some functions require polishing (mostly on adding proper error messsaging)
- [ ] edge cases of fptk macros interactions require more testing

Stable release is not yet reached, small API-breaking changes can happen.

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Installation ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Installation

The easiest way to install fptk is via command:
```
pip install git+https://github.com/rmnavr/fptk.git@main
```

<!-- __________________________________________________________________________/ }}}1 -->

