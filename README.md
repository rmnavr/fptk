
<!-- Intro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# **fptk** — functional toolkit for hy-lang

**fptk** is curated list of math and FP-related modules/classes/functions/macroses,
that are supposed to be imported into main scope altogether to always have them at your fingertips.
> Having high amount of functions available in main context is inspired by Wolfram Language,
> in which all (yes, ALL) standard functions are always in the main context.

Intended usage of **fptk** in hy is:
```hy
(import fptk *)     ; import modules, functions and classes
(require fptk *)    ; import macros
```

You can also use fptk in python, but you won't be able to access macros,
since macros are hy-only feature (even without macros fptk has many things to offer):
```hy
import fptk *
```

<!-- __________________________________________________________________________/ }}}1 -->

<!-- Topics ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Topics that fptk covers

**fptk** aims to unite and bring into main scope:
- [x] Full modules that are math and FP-relevant (math, operator, itertools, ...)
- [x] Buffed getters (first, second, last, rest, ...)
- [x] 1-based index variants of basic getters (don't worry, fptk does not enforce using them)
- [x] macros for lenses (lenses is Haskell-ish approach for manipulating deeply nested immutable data)
- [x] APL-like functions for mapping, filtering, partitioning, etc.
- [x] Functional composition/piping/currying 
- [x] Math, random generators, logic/checks
- [x] Strings manipulation and regexes 
- [x] Basic types like Any, Optional, etc.

Currently under development:
- [ ] Utils for benchmarking and debug
- [ ] Optional strict type checking (via pydantic)
- [ ] Immutable structures (source lib is under consideration)
- [ ] Monadic machinery (via returns)

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Dependencies ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Dependencies

Tested with:
* hy 1.0.0
* [funcy](https://github.com/Suor/funcy/) 2.0 (lib is used for FP functions)
* [pydantic](https://github.com/pydantic/pydantic) 2.0 (lib is used for type checking)
* [lenses](https://github.com/ingolemo/python-lenses) 1.2 (lib is used for, well, lenses)
* [returns](https://github.com/dry-python/returns) 0.23 (lib will probably be used for monadic machinery)

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Documentation ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## Documentation

1. [Functions and modules](https://github.com/rmnavr/fptk/blob/main/docs/functions.md) — auto-generated table with concise description for each entity
2. [Basic macros](https://github.com/rmnavr/fptk/blob/main/docs/macros.md) — every fptk macro except lens-related macros
3. [Lens related macros](https://github.com/rmnavr/fptk/blob/main/docs/lens.md) — macros that simplify lens definition/application/composition

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Installation ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Installation

You can install fptk via command:
```
pip install git+https://github.com/rmnavr/fptk.git@0.2.1
```

But for now (until fptk reaches stable release) recommended approach is to
download [_fptk_local.hy](https://github.com/rmnavr/fptk/blob/main/packaging_helper/generated_fptk_local/_fptk_local.hy)
file and place it into your project folder.

Then just add to your code:
```hy
(import  _fptk_local *)     ; import modules, functions and classes
(require _fptk_local *)    ; import macros
```

This way you will be protected from API breaking changes in future fptk versions.

<!-- __________________________________________________________________________/ }}}1 -->

