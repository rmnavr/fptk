
---
fptk functions, macroses and imported modules:
1. [Functions and modules](https://github.com/rmnavr/fptk/blob/main/docs/functions.md)
2. You are here -> [Basic macros](https://github.com/rmnavr/fptk/blob/main/docs/macros.md)
3. [Lens related macros](https://github.com/rmnavr/fptk/blob/main/docs/lens.md)
---

<!-- Intro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# fptk macros

This doc will cover following macros:
* `f::` — macro for annotating callables
* `fm`, `f>`, `lmapm` — anonymous functions with special syntax for arguments
* `p:` — pipe of partials
* `pluckm` — getter for collection of collections
* `getattrm` — same as getattr, but with small syntax tweak
* `assertm`, `gives_error_typeQ` — macros for testing code

There are also lens related macros described in separate doc
([Lens related macros](https://github.com/rmnavr/fptk/blob/main/docs/lens.md)).

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
(fm (* %1 2))               ; -> (fn [%1] (* %1 2))
(f> (* %1 %2 10) 3 4)       ; -> ((fn [%1 %2] (* %1 %2 10) 3 4)
(lmapm (pow %1 2) [1 2 3]   ; -> (list (map (fn [%1] (pow %1 2)) [1 2 3]))

; Just as original fn, fm will also work correctly with non-() forms:
(fm abs)                    ; -> (fn [] abs) 
(fm [%1 (str %2)])          ; -> (fn [%1 %2] [%1 (str %2)])
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
               (fn [x] x)            ; -4    // fn and fm-macro can be used with p:
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

The only thing this macros does is allowing `.attr` syntax for getattr
(to be more consistent with fptk syntax):

```hy
(getattrm Point "x")     ; -> (getattr Point "x")
(getattrm Point .x)      ; -> (getattr Point "x")
```

<!-- __________________________________________________________________________/ }}}1 -->
<!-- assertm, gives_error_typeQ ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## `assertm`

```hy
assertm(op, arg1, arg2) -> bool | Error
```

Checks if `(op arg1 arg2)` returns True and also:
1. Prints source expressions when check fails
2. Will return:
   - True when checks is true
   - False when check is False
   - Error object when check was not able to be calculated

```
(assertm = (+ 1 2) (- 3 10))
; Error in '(= (+ 1 2) (- 3 10)) | <class 'AssertionError'> : False
; >> '(+ 1 2) = 3
; >> '(- 3 10) = -7

(assertm = (+ z1z) (- 7))
; Error in '(= (+ z1z) (- 7)) | <class 'NameError'> : name 'z1z' is not defined
; >> Can't calc '(+ z1z) | <class 'NameError'> : name 'z1z' is not defined
; >> '(- 7) = -7
```

## `gives_error_typeQ`

```hy
gives_error_typeQ(expr, error_type) -> bool
```

Returns True when calculating `expr` produces error of `error_type`.
Returns False otherwise (eather when `expr` calculates without error, or when error type does not match).

Examples:
```hy
(gives_error_typeQ (get [1 2 3] 10) IndexError)
; True

(gives_error_typeQ (get [1 2 3] 1) IndexError)
; False
```

Best used with `assertm`:
```hy
(assertm gives_error_typeQ (get [1 2 3] 1) IndexError)
; Error in '(gives_error_typeQ (get [1 2 3] 1) IndexError) | <class 'AssertionError'> : False
; >> '(get [1 2 3] 1) = 2
; >> 'IndexError = <class 'IndexError'>
```

<!-- __________________________________________________________________________/ }}}1 -->

