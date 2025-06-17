
# **fptk** — functional toolkit for hy-lang

<!-- Intro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

**fptk** serves 2 distinct purposes.

*First purpose* is to provide several macros relevant for functional programming.

*Second purpose* is to tune hy/py to my own programming preferences (most of my hy-github projs rely on this lib).
Intended usage of fptk is bringing curated list of math/FP-related libs/classes/functions/macroses into main scope.

> Having high amount of functions available in main context is inspired by Wolfram Language, in which ALL standard functions are in the main context.

It is easier to see full list of imported entities directly inside
[fptk.hy](https://github.com/rmnavr/fptk/blob/main/fptk/fptk.hy) file (it is nicely organized).

Both purposes are achieved simply by calling:
```hy
(import fptk *)
(require * :readers *)
```

Table of Content:
- [topics that fptk covers](#Topics-that-fptk-covers)
- [fptk macros](#fptk-macros)
- [Installation](#Installation)

<!-- __________________________________________________________________________/ }}}1 -->

# Topics that fptk covers

<!-- topics ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

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

<!-- __________________________________________________________________________/ }}}1 -->
<!-- #L, fm, fm> ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `#L` — reader macro for writing lambdas

`#L` is just synonim for hyrule's `#%` macro.
```hy
#L(* %1 2) ; equivalent to: (fn [%1] (* %1 2))
```

## `fm` and `fm>` — macro for writing lambdas

```hy
(fm  (* %1 2))         ; expands to: (fn [%1] (* %1 2))
(fm> (* %1 %2 10) 3 4) ; immediately applicates — will give here: 3*4*10 = 120
```

`fm` and `fm>` have functionality similar to `#L`, but:
- unlike `#L`, `fm` and `fm>` are REPL-friendly
- `fm` and `fm>` currently support only args of form `%1`..`%9` (while `#L` can also work with args and kwargs)

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

Notice that unlike in `->` macro, `.x` is seen as attribute access rather than method call.

<!-- __________________________________________________________________________/ }}}1 -->
<!-- pluckm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `pluckm` — unification of lpluck/lpluck_attr funcs from funcy libs

Pluckm extends [funcy.lpluck](https://funcy.readthedocs.io/en/stable/colls.html#pluck)
to be able to recognize `(pluckm .attr)` syntax for accessing attributes.

Reason for this is the following:
* In hy original syntax `.attr` is the way to access attribute, not `"attr"` (how lpluck_attr does it)
* Attribute's names are rarely passed as parameter (it may be considered anti-pattern)

```hy
(pluckm 0       xs) ; expands to: (lpluck      0       xs)
(pluckm i       xs) ; expands to: (lpluck      i       xs)
(pluckm (- 1 1) xs) ; expands to: (lpluck      (- 1 1) xs)
(pluckm "key"   ds) ; expands to: (lpluck      "key"   ds)
(pluckm .attr   cs) ; expands to: (lpluck_attr "attr"  cs)

; use (lpluck_attr "attr" cs) if attribute name is needed to be passed as parameter
```

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Lenses ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## lenses-related macroses: `lns`, `&+`, `&+>`, `l>`, `l>=`

Those macroses should be used together with [lenses](https://github.com/ingolemo/python-lenses) library.
They simplify lens definition, composition and application.

### `lns` macro in details

`lns` macro offers alternative syntax for lenses.

Basic syntax:
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

`lns` macro also recognizes 4 special forms (`mth>` `mut>` `dndr>` `dndr>>`):
```
(lns 1 (mth> .sort))                ; (. lens (call "sort")
(lns 1 (mut> .copy :shallow True))  ; (. lens (call_mut "copy" :shallow True)
(lns 1 (dndr>  / 3))                ; (/ (. lens [1]) 3)
(lns 1 (dndr>> / 3))                ; (/ 3 (. lens [1]))

; mth> and mut> can't pass method as argument, use original (call "mth" ..) or (call_mut "mth" ..) instead
```

Reasoning for creating syntax (```.attr``` and ```(mth> .sort)```) that can't pass attr/method name as argument is the same as for `pluckm` macro:
* In hy original syntax `.smth` is the way to access attribute/method, not "smth"
* Attribute's names are rarely passed as parameter (it may be considered anti-pattern)

### all lenses macros

Most important objects in lens are UnboundLens and StateFunction, which will be further referred to as UL and SF.

For context, original lens library offers `&` and `&=` functions used like so:
```hy
(& (. lens [1]) (. lens [2] (set 3)))        ; lens composition (ULs + last one can be UL/SF)
(& xs (. lens [1] (get)) (. lens [2] (get))) ; SFs application (one after another)
(&= xs (. lens (Each) (modify sqrt)))        ; applicating SF to xs and updating xs value
```

Symbols in fptk lens macros names in general mean:
- `l` — understands `lns` macro syntax
- `&` — combine
- `+` — expects getter/setter at the end
- `>` — apply

fptk lens macros usages:
```
; lns is used to define UL or SF:
(lns 1 (Each))                      ; (. lens [1] (Each))
(lns 1 (Each) (set 3))              ; (. lens [1] (Each) (set 3))

; l> is used to apply SF, l>= also updates value:
(l>  xs 1 (Each) (modify sqrt))     ; ((. lens [1] (Each) (modify sqrt)) xs)
(l>= xs 1 (Each) (modify sqrt))     ; (setv xs ((. lens [1] (Each) (modify sqrt)) xs))

; &+ can combine several ULs and expects getter/setter at the end
; &+> also applies composed SF:
(&+ (lns 1) (lns 2) (set "here"))   ; (& (. lens [1]) (. lens [2] (set "here")))
(&+> xs (lns 1) (mut> .sort))       ; ((& (. lens [1] (call_mut "sort"))) xs)
```

<!-- __________________________________________________________________________/ }}}1 -->

# Installation

```
pip install git+https://github.com/rmnavr/fptk.git@0.0.1
```
