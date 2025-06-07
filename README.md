**hyfptk** — functional toolkit for hy-lang.

My personal intended usage of hyfptk is to call:
```hy
(import fptk *)
(require fptk *)
```
which will import curated list of libs, classes, functions and macroses into main context.
> This approach is inspired by Wolfram Mathematica, in which all language functions are always in the main context.
Still, nothing forbids you from importing functions individually.

**hyfptk** contains of:
* curated list of basic and fp-related libs (from math to [funcy](https://github.com/ingolemo/python-lenses) and [lenses](https://github.com/ingolemo/python-lenses))
* curated list of functions (like `sin`, `flip`, `first` and many others)
* functions for basic operators (like `plus` for `+` and such)
* some new hy fp-related macroses

# Macroses 

* f::
* #L
* fm
* p>
* lens-related macroses: lns, &+, &+>, l>, l>=
* pluckm
