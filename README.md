
<!-- Intro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# **fptk** — functional toolkit for hy-lang

**fptk** is curated list of math and FP-related modules/classes/functions/macroses,
that are supposed to be imported into main scope altogether to always have them at your fingertips.
> Having high amount of functions available in main context is inspired by Wolfram Language,
> in which all (yes, ALL) standard functions are always in the main context.

Intended usage of **fptk** is:
```hy
(import fptk *)		; import of modules, functions and classes
(require fptk *)	; import of macros
```

---

Table of Contents:
- [Topics that fptk covers](#Topics-that-fptk-covers)
- [fptk imports](#fptk-imports)
- [fptk macros](#fptk-macros)
- [Installation](#Installation)

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Topics ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Topics that fptk covers

**fptk** aims to unite and bring into main scope:
- [x] full modules that are math and FP-relevant (math, operator, itertools, ...)
- [x] Buffed getters (first, second, last, rest, ...)
- [x] 1-based index variants of basic getters (I can't stand slicing with 0-based index)
- [x] [lenses](https://github.com/ingolemo/python-lenses) for manipulating deeply nested immutable data
- [x] APL-like functions for mapping, filtering, partitioning, etc. (source libs: funcy, itertools, etc.)
- [x] Functional composition/piping/currying (source libs: hyrule, funcy, itertools, etc.)
- [x] Math, random generators, logic/checks
- [x] Strings manipulation and regexes 
- [x] Basic typing (Any, Optional, ...)

Currently under development:
- [ ] Utils for benchmarking and debug
- [ ] Type checking (source libs: [pydantic](https://github.com/pydantic/pydantic))
- [ ] Immutable structures (source libs: under consideration)
- [ ] Monadic machinery (source libs: [returns](https://github.com/dry-python/returns))

<!-- __________________________________________________________________________/ }}}1 -->

<!-- Functions ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# fptk imports

**fptk** imports into main scope whole modules, functions and classes (and macroses, but they are described separately below).
Whole list of these things is given in auto-generated table:
[doc_generator/output/README.md](https://github.com/rmnavr/fptk/blob/main/doc_generator/output/README.md)

<!-- __________________________________________________________________________/ }}}1 -->

<!-- Macros intro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# fptk macros

Full list of fptk macros:
* `f::` — macro for annotating callables
* `#L`, `fm`, `f>` — anonymous functions
* `p:` — pipe of partials
* `pluckm` — getter for collection of collections
* `getattrm` — same as getattr, but with small syntax tweak
* `lns`, `&+`, `&+>`, `l>`, `l>=` — macros for lenses definition/composition/application

<!-- __________________________________________________________________________/ }}}1 -->
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
<!-- fm, f>, lmapm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `fm`, `f>`, `lmapm` — macros for writing lambdas

Following macros all have same similar `%i` arguments recognition:
- `fm` defines lambda
- `f>` defines and immediately applicates lambda
- `lmapm` is "list map" that takes lambda for func of map

```hy
(fm (* %1 2))				; -> (fn [%1] (* %1 2))
(f> (* %1 %2 10) 3 4)     	; -> ((fn [%1 %2] (* %1 %2 10) 3 4)
(lmapm (pow %1 2) [1 2 3] 	; -> (list (map (fn [%1] (pow %1 2)) [1 2 3]))

; Just as original fn, fm will also work correctly with non-() forms:
(fm abs)					; -> (fn [] abs) 
(fm [%1 (str %2)])			; -> (fn [%1 %2] [%1 (str %2)])
```

`fm` (and friends) has functionality similar to hyrule's `#%` reader macro, but:
- `fm` is more REPL-friendly than `#%` in my setup (I use [hy-ipython](https://pypi.org/project/hy-ipython/) with hy 1.0.0, despite lib saying it needs exactly hy 0.24)
- `fm` currently support only args of form `%1`..`%9` (while `#%` reader macro can also work with args and kwargs)

<!-- __________________________________________________________________________/ }}}1 -->
<!-- p: ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `p:` — pipe of partials

Works similar to hyrule `->` macro, but accepts only callables and does not need to be called immediately.
Internally piping is implemented via partial application with [funcy.partial](https://funcy.readthedocs.io/en/stable/funcs.html#partial).

Example:
```hy
(setv x 4)
                                     ; after application to x will produce at each step:
(setv pipe (p: operator.neg          ; -4
			   (fn [x] x)			 ; -4    // fn and fm-macro can be used with p:
               (abs)                 ; 4
               (operator.add 4)      ; 8
               str                   ; '8'
               (.__contains__ "8")   ; True  // demonstration of method call
               .__class__            ; 'str' // demonstration of attribute access
               ))

(print (pipe x))                     ; returns <class 'str'>
```

Notice that unlike in `->` macro, `.attr` is seen as attribute access rather than method call.
This is also in accordance with `.attr` usage inside another fptk macros.

<!-- __________________________________________________________________________/ }}}1 -->
<!-- pluckm, getattrm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `pluckm` — unification of lpluck/lpluck_attr funcs from funcy libs

Pluckm extends [funcy.lpluck](https://funcy.readthedocs.io/en/stable/colls.html#pluck)
to be able to recognize `(pluckm .attr)` syntax for accessing attributes.

Reason for adding such syntax is the following:
* In hy original syntax `.attr` is the way to access attribute, not `"attr"` (how lpluck_attr does it)
* Attribute's names are rarely passed as parameter (it may be considered anti-pattern)

```hy
; .attr syntax expands to lpluck_attr:
(pluckm .attr cs)   ; (lpluck_attr "attr" cs)
; [!] notice that .attr syntax can't pass attr as argument,
;     use (lpluck_attr "attr" cs) instead

; everything else is expanded to lpluck:
(pluckm (+ i 3) xs) ; (lpluck (+ i 3) xs)
(pluckm "key" xs)   ; (lpluck "key" xs)
```


<!-- __________________________________________________________________________/ }}}1 -->
<!-- getattrm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `getattrm`

The only thing this macros does is allows `.attr` syntax for getattr
(to be more consistent with fptk syntax):

```hy
(getatrm Point "x")		; -> (getattr Point "x")
(getatrm Point .x)		; -> (getattr Point "x")
```

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Lenses ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## lenses-related macroses: `lns`, `&+`, `&+>`, `l>`, `l>=`

These macros should be used together with [lenses](https://github.com/ingolemo/python-lenses) library.
They simplify lens definition, composition and application.
> Lenses is Haskell-inspired library for working with deeply nested immutable data.

### `lns` macro in details

`lns` macro offers alternative syntax for lenses.

Basic syntax of `lns` macro:
```hy
; standalone numbers, strings and variables are recognized as index access:
(lns 1 -2 (- 7) "key" idx)  ; (. lens [1] [-2] [- 7] ["key"] [idx])

; form below is seen as attribute access:
(lns .attr)                 ; (. lens attr)
; [!] notice that .attr form can't pass attr as argument,
;     use (GetAttr "attr") instead

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

; [!] notice that mth> and mut> can't pass method as argument,
;     use (call "mth" ..) or (call_mut "mth" ..) instead
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

For context, original lenses library offers `&` and `&=` functions that are used like so:
```hy
(& (. lens [1]) (. lens [2] (set 3)))        ; lens composition (ULs + last one can be UL/SF)
(& xs (. lens [1] (get)) (. lens [2] (get))) ; SFs application (one after another)
(&= xs (. lens (Each) (modify sqrt)))        ; applicating SF to xs and updating xs value
```

fptk lens macros (`l>`, `l>=`, `&+` and `&+>`) do not aim to replace original `&` and `&=` functions.
They aim to extend number of ways in which lenses can be composed sintactically.
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

<!-- Installation ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Installation

```
pip install git+https://github.com/rmnavr/fptk.git@0.2.0
```

<!-- __________________________________________________________________________/ }}}1 -->

