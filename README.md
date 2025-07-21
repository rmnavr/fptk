
<!-- Intro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# **fptk** — functional toolkit for hy-lang

fptk is curated list of math and FP-related modules/classes/functions/macroses,
that are supposed to be imported into main scope altogether to always have them at your fingertips.
> Having high amount of functions available in main context is inspired by Wolfram Language,
> in which all (yes, ALL) standard functions are always in the main context.

Intended usage of fptk in hy is:
```hy
(import fptk *)     ; import modules, functions and classes
(require fptk *)    ; import macros
```

You can also use fptk in python, but you won't be able to access macros,
since macros are hy-only feature (even without macros, fptk functions can be of interest):
```python
import fptk *
```

<!-- __________________________________________________________________________/ }}}1 -->

<!-- Topics ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Topics that fptk covers

fptk promotes approaches:
* provide user with set of convenience functions
  like `read_file(file_name, encoding='utf-8')`, that are streightforward to use and compose
* no data mutation
* taking high inspiration from  lib, fptk
  tries to make even clear namings (like `split` -> `filter_split`, `split_by` -> `bisect_by`, etc.)
* most checks have Q postfix, like `intQ`, `zerolenQ`, etc.

fptk aims to unite and bring following things into main scope.

Basics:
- [x] Full basic modules that are math and FP-relevant (`math`, `operator`, `itertools`, etc.)
- [x] Math, logic and checks (most checks in fptk end with "Q": like `intQ`, `zerolenQ`, etc.)
- [x] Strings manipulation and regexes
- [x] Convenience functions for basic IO (functions like `read_file` and `file_existsQ`)
- [ ] Basic benchmarking and debug

Typing:
- [x] Basic types like `Any`, `Optional`, etc.
- [x] Strict type checking via [pydantic](https://github.com/pydantic/pydantic)
      (for example decorator `validateF` will validate arguments and return value of function)

APL-like functionality (heavily based on [funcy](https://github.com/Suor/funcy/) lib):
> fptk follows funcy approach of providing both generator and list version of most functions (like `map` and `lmap`)
- [x] Buffed getters (first, second, last, rest, ...)
- [x] Functional composition, piping, partial application and currying
- [x] Mapping, filtering and partitioning of sequences

Other quirky things:
- [x] 1-based index variants of basic getters (don't worry, fptk does not force using them)
- [x] macros for lenses ([lenses](https://github.com/ingolemo/python-lenses) is Haskell-ish approach for manipulating deeply nested immutable data)

Currently under development:
- [ ] Immutable structures (source lib is under consideration)
- [ ] Monadic machinery (considering using [returns](https://github.com/dry-python/returns) lib)

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Dependencies ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Dependencies

Tested with:
* [hy](https://github.com/hylang/hy) 1.0.0
* [funcy](https://github.com/Suor/funcy/) 2.0
* [pydantic](https://github.com/pydantic/pydantic) 2.0
* [lenses](https://github.com/ingolemo/python-lenses) 1.2 
* [returns](https://github.com/dry-python/returns) 0.23 

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Documentation ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## Documentation

1. [Functions and modules](https://github.com/rmnavr/fptk/blob/main/docs/functions.md) — auto-generated table with concise description for each entity
2. [Basic macros](https://github.com/rmnavr/fptk/blob/main/docs/macros.md) — every fptk macro except lens-related macros
3. [Lens related macros](https://github.com/rmnavr/fptk/blob/main/docs/lens.md) — macros that simplify lens definition/application/composition

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Installation ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Installation

The easiest way to install fptk is via command:
```
pip install git+https://github.com/rmnavr/fptk.git@0.2.1
```

But for now (until fptk reaches stable release) recommended approach is to
download [_fptk_local.hy](https://github.com/rmnavr/fptk/blob/main/packaging_helper/generated_fptk_local/_fptk_local.hy)
file and place it into your project folder.

Then just add to your code:
```hy
(import  _fptk_local *)    ; import modules, functions and classes
(require _fptk_local *)    ; import macros
```

This way you will be protected from API breaking changes in future fptk versions.

<!-- __________________________________________________________________________/ }}}1 -->

