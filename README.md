# **fptk** — functional toolkit for hy-lang

Intended usage of hyfptk is to call:
```hy
(import fptk *)
(require fptk *)
```
which will import curated list of FP-relevant libs, classes, functions and macroses into main context.
> This approach is inspired by Wolfram Mathematica, in which all language functions are always in the main context.
> Still, nothing forbids you from importing functions individually.

Other possible usage (if you don't want to pollute main context with all the functions) is importing only fptk macroses like:
```hy
(require fptk [f:: p> fm lns &+ &+> l> l>= pluckm] :readers [L])
```

**fptk** contains of:
* curated list of basic and fp-related libs (from itertools/math/random to [funcy](https://github.com/Suor/funcy) and [lenses](https://github.com/ingolemo/python-lenses))
* curated list of functions (like `sin`, `flip`, `first` and many others)
* curated list of hyrule macroses
* functions for basic operators (like `plus` for `+` and such)
* some new hy fp-related macroses

# fptk Macroses

## `f::` — macros for annotating Callables
```hy
(f:: int -> int => (of Tuple int str))
; which is equivalent of py-code: Callable[[int, int], Tuple[int, str]]
```

## `#L` — reader macro synonim for hyrule `#%` macro

`#L` is macro for writing lambdas like: 
```hy
#L(* %1 2)
```

## `fm` — normal macro with functionality same to `#L`

Unlike `#L`, `fm` is REPL-friendly:
```hy
(fm (* %1 2))
```

## `p>` — pipe of partials

Works similar to hyrule `->` macro, but accepts only functions.
Functions are partially applicated using funcy.partial.

Example:
```hy
(list (map (p> abs (operator.add 4) str) (range -10 0)))
```

## `pluckm` — unification of lpluck/lpluck_attr funcs from funcy libs

```hy
(pluckm 1  xs) ; will pluck index 1 from xs
(pluckm .x cs) ; will pluck "x" attr from cs
```

## lenses-related macroses: `lns`, `&+`, `&+>`, `l>`, `l>=`

```hy
(lns 1 "dict" vrbl .attr (Each))           ; attr   can't be passed as arg, use (GetAttr  "attr") if needed
(lns 1 (mth> .sort))                       ; method can't be passed as arg, use (call     "mth" ..) if needed
(lns 1 (mut> .sort :shallow True))         ; method can't be passed as arg, use (call_mut "mth" ..) if needed
(lns 1 (dndr>  / 1))                
(lns 1 (dndr>> / 1))                       

(lns 1 (Each) (set 3))                     ; can define UL/SF
(l>  xl 1 (Each) (modify sqrt))            ; define SF and apply
(l>= xl 1 (Each) (modify sqrt))            ; define SF, apply, upd value
(&+  (lns 1) (lns 2) (set "here"))         ; / compose ULs and «SF-maker-func» ...
(&+> xl (lns 1) (mut> .reverse))           ; \ .. and then apply
```

# Installation

```
pip install git+https://github.com/rmnavr/fptk.git@0.0.1
```
