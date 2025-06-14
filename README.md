
# **fptk** — functional toolkit for hy-lang

<!-- Intro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

**fptk** provides several usefull macroses for hy: 
```hy
(require fptk [f:: p> pluckm])
(require fptk [lns &+ &+> l> l>=])
(require fptk [fm] :readers [L])
```
*(see macroses description below)*

However main purpose of **fptk** is to tune hy/py to my own programming preferences (most of my hy-github projs rely on this lib).
Supposed usage of fptk is bringing curated list of math/FP-related libs/classes/functions/macroses into main context by calling:
```hy
(import fptk *)
(require fptk * :readers *)
```

It is easier to see full list of imported entities directly inside `fptk.hy` file (it is nicely organized).

> Having high amount of functions available in main context is inspired by Wolfram Language, in which ALL standard functions are in the main context.

Overview of things **fptk** aims to unite and bring into main context:
- [x] Basic math (math)
- [x] Regexes (re, [funcy](https://github.com/Suor/funcy))
- [x] Standard FP/APL features like mapping/filtering/threading (funcy, itertools)
- [x] Functional composition/piping/currying (hyrule, funcy, itertools)
- [x] Lenses for manipulating deeply nested immutable data ([lenses](https://github.com/ingolemo/python-lenses))
- [ ] WIP: Types checking during creating objects and calling functions ([pydantic](https://github.com/pydantic/pydantic))
- [ ] WIP: Immutable structures (under consideration)
- [ ] WIP: Monadic machinery ([returns](https://github.com/dry-python/returns))

<!-- __________________________________________________________________________/ }}}1 -->

# fptk Macroses

<!-- f:: ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `f::` — macros for annotating Callables
Used for annotating functions.
One possible usage might be defining interfaces (if functions are used in that role).

```hy
(setv ICaller (f:: int -> int => (of Tuple int str)))
; equivalent py-code: ICaller = Callable[[int, int], Tuple[int, str]]
```

Inside `f::` macro, symbols `->` (and `=>`) are recognized just as argument separator rather than hyrule's macro `->`.

<!-- __________________________________________________________________________/ }}}1 -->
<!-- #L, fm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `#L` — reader macro for writing lambdas

`#L` is just synonim for hyrule's `#%` macro. 
```hy
#L(* %1 2)
```

## `fm` — normal macro for writing lambdas

```hy
(fm (* %1 2))
```

`fm` has functionality similar to `#L`, but:
- unlike `#L`, `fm` is REPL-friendly
- `fm` currently supports only args of form `%1`..`%9` (while `#L` can also work with args and kwargs)

<!-- __________________________________________________________________________/ }}}1 -->
<!-- p> ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `p>` — pipe of partials

Works similar to hyrule `->` macro, but accepts only callables and does not need to be called immediately.
Internally piping is implemented via partial application with [funcy.partial](https://funcy.readthedocs.io/en/stable/funcs.html#partial).

Example:
```hy
(setv pipe (p> abs
               (operator.add 4)
               (operator.truediv 4)
               str))
(list (map pipe (range -10 0)))
```

<!-- __________________________________________________________________________/ }}}1 -->
<!-- pluckm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `pluckm` — unification of lpluck/lpluck_attr funcs from funcy libs

Pluckm extends [funcy.lpluck](https://funcy.readthedocs.io/en/stable/colls.html#pluck)
to be able to recognize `(pluckm .attr)` syntax for accessing attributes.

Reason for this is the following:
* In hy original syntax `.attr` is the way to access attribute, not `"attr"` (how lpluck_attr does it)
* Attribute's names are rarely passed as parameter (it may be considered anti-pattern)

```hy
(pluckm 0       xs) ; (lpluck      0       xs)
(pluckm i       xs) ; (lpluck      i       xs)
(pluckm (- 1 1) xs) ; (lpluck      (- 1 1) xs)
(pluckm "key"   ds) ; (lpluck      "key"   ds)
(pluckm .attr   ps) ; (lpluck_attr "attr"  ps) ; use lpluck_attr if attribute name is needed to be passed as parameter
```

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Lenses ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## lenses-related macroses: `lns`, `&+`, `&+>`, `l>`, `l>=`

Those macroses should be used together with [lenses](https://github.com/ingolemo/python-lenses) library.
They simplify lens definition, composition and application.
Macroses `l>` and `&+>` work best together with hyrule `->` macro.

```hy
(lns 1 "dict" vrbl .attr (Each))           ; attr   can't be passed as arg, use (GetAttr  "attr") if needed
(lns 1 (mth> .sort))                       ; method can't be passed as arg, use (call     "mth" ..) if needed
(lns 1 (mut> .sort :shallow True))         ; method can't be passed as arg, use (call_mut "mth" ..) if needed
(lns 1 (dndr>  / 1))                
(lns 1 (dndr>> / 1))                       

(lns 1 (Each) (set 3))                     ; can define UL/SF
(l>  xs 1 (Each) (modify sqrt))            ; define SF and apply
(l>= xs 1 (Each) (modify sqrt))            ; define SF, apply, upd value
(&+  (lns 1) (lns 2) (set "here"))         ; / compose ULs and «SF-maker-func» ...
(&+> xs (lns 1) (mut> .reverse))           ; \ .. and then apply
```

<!-- __________________________________________________________________________/ }}}1 -->


<!-- TODO:

    - p>     upd/docs
    - lenses upd/docs

-->

# Installation

```
pip install git+https://github.com/rmnavr/fptk.git@0.0.1
```
