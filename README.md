
# **fptk** — functional toolkit for hy-lang

<!-- Intro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

**fptk** serves 2 distinct purposes.

First purpose is to provide several macros relevant for functional programming.

Second purpose is to tune hy/py to my own programming preferences (most of my hy-github projs rely on this lib).
Intended usage of fptk is bringing curated list of math/FP-related libs/classes/functions/macroses into main scope.

> Having high amount of functions available in main context is inspired by Wolfram Language, in which ALL standard functions are in the main context.

It is easier to see full list of imported entities directly inside
[fpext.hy](https://github.com/rmnavr/fptk/blob/main/fptk/fpext.hy) file (it is nicely organized).

Both purposes are achieved simply by calling:
```hy
(import fptk *)
(require * :readers *)
```

---

Table of Contents:
- [topics that fptk covers](#Topics-that-fptk-covers)
- [fptk macros](#fptk-macros)
- [Installation](#Installation)

<!-- __________________________________________________________________________/ }}}1 -->

# Topics that fptk covers

<!-- topics ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

As a FP-tuning lib, fptk aims to unite and bring into main scope:
- [x] Basic math (math)
- [x] Regexes (re, [funcy](https://github.com/Suor/funcy))
- [x] Standard FP/APL features like mapping/filtering/threading (funcy, itertools)
- [x] Functional composition/piping/currying (hyrule, funcy, itertools)
- [x] Lenses for manipulating deeply nested immutable data ([lenses](https://github.com/ingolemo/python-lenses))
- [ ] WIP: Types checking during creating objects and calling functions ([pydantic](https://github.com/pydantic/pydantic))
- [ ] WIP: Immutable structures (under consideration)
- [ ] WIP: Monadic machinery ([returns](https://github.com/dry-python/returns))

<!-- __________________________________________________________________________/ }}}1 -->

# fptk macros

<!-- f:: ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `f::` — macros for annotating Callables
Used for annotating functions.
One possible usage might be defining interfaces (if functions are used in that role).

```hy
(setv ICaller (f:: int -> int => (of Tuple int str)))
; equivalent py-code: ICaller = Callable[[int, int], Tuple[int, str]]
```

Inside `f::` macro, symbols `->` (and `=>`) are recognized just as argument separator rather than hyrule's macro `->`.
Also, `->` can be used instead of last `=>` (this is simply visual preference and has no impact on the code).

<!-- __________________________________________________________________________/ }}}1 -->
<!-- #L, fm, f> ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `#L` — reader macro for writing lambdas

`#L` is just synonim for hyrule's `#%` macro.
```hy
#L(* %1 2) ; equivalent to: (fn [%1] (* %1 2))
```

## `fm` and `f>` — macro for writing lambdas

```hy
(fm (* %1 2))         ; expands to: (fn [%1] (* %1 2))
(f> (* %1 %2 10) 3 4) ; immediately applicates — will give here: 3*4*10 = 120
```

`fm` and `f>` have functionality similar to `#L`, but:
- `fm` and `f>` are more REPL-friendly in my setup (I use [hy-ipython](https://pypi.org/project/hy-ipython/) with hy 1.0.0, despite lib saying it needs exactly 0.24), than `#L`
- `fm` and `f>` currently support only args of form `%1`..`%9` (while `#L` can also work with args and kwargs)

<!-- __________________________________________________________________________/ }}}1 -->
<!-- p> ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `p>` — pipe of partials

Works similar to hyrule `->` macro, but accepts only callables and does not need to be called immediately.
Internally piping is implemented via partial application with [funcy.partial](https://funcy.readthedocs.io/en/stable/funcs.html#partial).

Example:
```hy
(setv x 4)
                                     ; after application to x will produce at each step:
(setv pipe (p> operator.neg          ; -4
               (abs)                 ; 4
               (operator.add 4)      ; 8
               str                   ; '8'
               (.__contains__ "8")   ; True  // this line demonstrates method call
               .__class__            ; 'str' // this line demonstrates attribute access
               ))

(print (pipe x))                     ; returns <class 'str'>
```

Notice that unlike in `->` macro, `.attr` is seen as attribute access rather than method call.
This is also in accordance with `.attr` usage inside another fptk macros.

<!-- __________________________________________________________________________/ }}}1 -->
<!-- pluckm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `pluckm` — unification of lpluck/lpluck_attr funcs from funcy libs

Pluckm extends [funcy.lpluck](https://funcy.readthedocs.io/en/stable/colls.html#pluck)
to be able to recognize `(pluckm .attr)` syntax for accessing attributes.

Reason for this is the following:
* In hy original syntax `.attr` is the way to access attribute, not `"attr"` (how lpluck_attr does it)
* Attribute's names are rarely passed as parameter (it may be considered anti-pattern)

```hy
; .attr syntax expands to lpluck_attr:
(pluckm .attr cs)   ; (lpluck_attr "attr" cs)
; notice that .attr syntax can't pass attr as argument, use (lpluck_attr "attr" cs) instead

; everything else is expanded to lpluck:
(pluckm (+ i 3) xs) ; (lpluck (+ i 3) xs)
(pluckm "key" xs)   ; (lpluck "key" xs)
```

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Lenses ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## lenses-related macroses: `lns`, `&+`, `&+>`, `l>`, `l>=`

These macros should be used together with [lenses](https://github.com/ingolemo/python-lenses) library.
They simplify lens definition, composition and application.

### `lns` macro in details

`lns` macro offers alternative syntax for lenses.

Basic syntax of `lns` macro:
```hy
; standalone numbers, strings and variables are recognized as index access:
(lns 1 -2 (- 7) "key" idx)  ; (. lens [1] [-2] [- 7] ["key"])

; form below is seen as attribute access:
(lns .attr)                 ; (. lens attr)
; notice that .attr form can't pass attr as argument, use (GetAttr "attr") instead

; arbitrary expressions (i.e. that are surrounded by parentheses) are parsed without changes:
(lns (Each) (set 3))        ; (. lens (Each) (set 3))

; thus, to ensure arbitrary expression is recognized as index, you must use square brackets:
(lns (+ i 3))               ; (. lens (+ i 3))   // will give error
(lns [(+ i 3)])             ; (. lens [(+ i 3)]) // works ok
```

`lns` macro also recognizes 4 special forms (`mth>`, `mut>`, `dndr>` and `dndr>>`):
```hy
(lns 1 (mth> .sort))                ; (. lens (call "sort")
(lns 1 (mut> .copy :shallow True))  ; (. lens (call_mut "copy" :shallow True)
(lns 1 (dndr>  / 3))                ; (/ (. lens [1]) 3)
(lns 1 (dndr>> / 3))                ; (/ 3 (. lens [1]))

; mth> and mut> can't pass method as argument, use (call "mth" ..) or (call_mut "mth" ..) instead
```

Reasoning for creating syntax (```.attr``` and ```(mth> .sort)```) that can't pass attr/method name as argument is the same as for `pluckm` macro:
* In hy original syntax `.smth` is the way to access attribute/method, not `"smth"`
* Attribute's names are rarely passed as parameter (it may be considered anti-pattern)

### All lenses macros

Most important objects in lenses are UnboundLens and StateFunction, which will be further referred to as UL and SF.
```hy
; lns can be used to define both UL or SF:
(lns 1 (Each))                      ; (. lens [1] (Each))
(lns 1 (Each) (set 3))              ; (. lens [1] (Each) (set 3))
```

For context, original lenses library offers `&` and `&=` functions used like so:
```hy
(& (. lens [1]) (. lens [2] (set 3)))        ; lens composition (ULs + last one can be UL/SF)
(& xs (. lens [1] (get)) (. lens [2] (get))) ; SFs application (one after another)
(&= xs (. lens (Each) (modify sqrt)))        ; applicating SF to xs and updating xs value
```

Those original functions `&` and `&=` are accompanied by fptk lens macros (`l>`, `l>=`, `&+` and `&+>`) in the following manner:
```hy
; l> is used to apply SF, l>= also updates value:
(l>  xs 1 (Each) (modify sqrt))     ; ((. lens [1] (Each) (modify sqrt)) xs)
(l>= xs 1 (Each) (modify sqrt))     ; (setv xs ((. lens [1] (Each) (modify sqrt)) xs))

; &+ can combine several ULs and expects getter/setter at the end
; &+> also applies composed SF:
(&+ (lns 1) (lns 2) (set "here"))   ; (& (. lens [1]) (. lens [2] (set "here")))
(&+> xs (lns 1) (mut> .sort))       ; ((& (. lens [1] (call_mut "sort"))) xs)
```

So, in general, symbols in fptk macros names (`l>`, `l>=`, `&+` and `&+>`) mean:
- `l` — expects `lns` macro syntax
- `&` — combine
- `+` — expects getter/setter at the end
- `>` — apply

<!-- __________________________________________________________________________/ }}}1 -->

# Installation

```
pip install git+https://github.com/rmnavr/fptk.git@0.0.1
```
