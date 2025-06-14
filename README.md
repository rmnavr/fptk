
# **fptk** — functional toolkit for hy-lang

<!-- Intro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

**fptk** provides several usefull macroses for hy: 
```hy
(require fptk [f:: p> pluckm])
(require fptk [lns &+ &+> l> l>=])
(require fptk [fm] :readers [L])
```

However main purpose of **fptk** is to tune hy/py to my own programming preferences (most of my hy-github projs rely on this lib).
Supposed usage of fptk is bringing curated list of math/FP-related libs/classes/functions/macroses into main context by calling:
```hy
(import fptk *)
(require fptk * :readers *)
```

It is easier to see full list of imported entities directly inside fptk.hy file (it is nicely organized).

> Having high amount of functions available in main context is inspired by Wolfram Language, in which ALL standard functions are in the main context.

Overview of things **fptk** aims to unite and bring into main context:

It imports libs like itertools/math///etc. ,
- [x] Basic math (math)
- [x] Regexes (re, [funcy](https://github.com/Suor/funcy))
- [x] Standard FP/APL features like mapping/filtering/threading (funcy, itertools)
- [x] Functional composition/piping/currying (hyrule, funcy, itertools)
- [x] Lenses for manipulating deeply nested immutable data ([lenses](https://github.com/ingolemo/python-lenses))
- [ ] Types checking during creating objects and calling functions (([pydantic](https://github.com/pydantic/pydantic))
- [ ] Immutable structures (under consideration)
- [ ] Monadic machinery (([returns](https://github.com/dry-python/returns))

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

## `#L` — reader macro synonim for hyrule `#%` macro

`#L` is macro for writing lambdas like: 
```hy
#L(* %1 2)
```

## `fm` — normal macro with functionality same to `#L`

```hy
(fm (* %1 2))
```

Unlike `#L`, `fm` is REPL-friendly.

`fm` currently supports only args of form %1..%9 (while `#L` can also work with args and kwargs).

<!-- __________________________________________________________________________/ }}}1 -->
<!-- p> ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `p>` — pipe of partials

Works similar to hyrule `->` macro, but accepts only callables.
Internally piping is implemented via partial application with funcy.partial

Example:
```hy
(list (map (p> abs (operator.add 4) (flip div 4) str) (range -10 0)))
; BTW «flip» and «div» are functions from fptk
; - «flip» flips argument order for 2-argument function
; - «div» is operator.truediv
```

<!-- __________________________________________________________________________/ }}}1 -->
<!-- pluckm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `pluckm` — unification of lpluck/lpluck_attr funcs from funcy libs

Pluckm extends funcy.lpluck to recognize `(pluckm .attr)` syntax for accessing attributes.

Reason for this is the following:
* Attribute's names are rarely passed as parameter (it may be considered anti-pattern)
* In hy original syntax `.attr` is the way to access attribute, not `"attr"`

```hy
(pluckm 0       xs)     ; (lpluck      0       xs)
(pluckm i       xs)     ; (lpluck      i       xs)
(pluckm (- 1 1) xs)     ; (lpluck      (- 1 1) xs)
(pluckm "key"   ds)     ; (lpluck      "key"   ds)
(pluckm .attr   ps)     ; (lpluck_attr "attr"  ps) 
```

<!-- __________________________________________________________________________/ }}}1 -->

<!-- Lenses ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## lenses-related macroses: `lns`, `&+`, `&+>`, `l>`, `l>=`

Those macroses are used together with [lenses](https://github.com/ingolemo/python-lenses)) library.
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


# Installation

```
pip install git+https://github.com/rmnavr/fptk.git@0.0.1
```
