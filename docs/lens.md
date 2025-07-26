
---
fptk functions, macroses and imported modules:
1. [Functions and modules](https://github.com/rmnavr/fptk/blob/main/docs/functions.md)
2. [Basic macros](https://github.com/rmnavr/fptk/blob/main/docs/macros.md)
3. You are here -> [Lens related macros](https://github.com/rmnavr/fptk/blob/main/docs/lens.md)
---

# Lenses-related macroses

Lenses macros should be used together with [lenses](https://github.com/ingolemo/python-lenses) library.
They simplify lens definition, composition and application.
> Lenses is Haskell-inspired library for working with deeply nested immutable data

Following macros are used in fptk:
* `lns`
* `&+`
* `&+>`
* `l>`
* `l>=` 

In general, symbols in fptk macros names (`l>`, `l>=`, `&+` and `&+>`) mean:
- `l` — expects `lns` macro syntax
- `&` — combine
- `+` — expects getter/setter at the end
- `>` — apply

<!-- lns ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# `lns` macro in details

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

<!-- __________________________________________________________________________/ }}}1 -->
<!-- all macros ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# All lenses macros

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
They aim to extend number of ways in which lenses can be composed syntactically.
```hy
; l> is used to apply SF, l>= also updates value, both of them use lns syntax:
(l>  xs 1 (Each) (modify sqrt))     ; ((. lens [1] (Each) (modify sqrt)) xs)
(l>= xs 1 (Each) (modify sqrt))     ; (setv xs ((. lens [1] (Each) (modify sqrt)) xs))

; &+ can combine several ULs (in eather lns or normal syntax) and require getter/setter at the end,
; &+> also applies composed SF:
(&+ (lns 1) (. lens [2]) (set "here"))  ; (& (. lens [1]) (. lens [2] (set "here")))
(&+> xs (lns 1) (mut> .sort))           ; ((& (. lens [1] (call_mut "sort"))) xs)
```

<!-- __________________________________________________________________________/ }}}1 -->

