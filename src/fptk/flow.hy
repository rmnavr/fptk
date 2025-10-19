
; Import and export ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (export :objects [ constantly
                       identity partial rpartial
                       compose rcompose ljuxt flip pflip
                       lmap starmap lstarmap reduce reductions lreductions
                       sums lsums product
                       lzip
                       nested apply_n
                     ]
            :macros  [ case branch unless lif as-> -> ->> doto do_n list_n
                       p:                       ; fptk macros
                       fm f>                    ; fptk macros
                       mapm lmapm               ; fptk macros
                       filterm lfilterm         ; fptk macros
                     ])

    (require hyrule [comment])

; _____________________________________________________________________________/ }}}1

; [GROUP] FP: Control flow ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (comment "hy | base | if   | (if check true false)          | ")
    (comment "hy | base | cond | (cond check1 do1 ... true doT) | ")

    (require hyrule [case])   
    (require hyrule [branch])
    (require hyrule [unless])
    (require hyrule [lif])

; _____________________________________________________________________________/ }}}1
; [GROUP] FP: Composition ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import  hyrule [constantly])    #_ "constantly(val) | constantly(30) is FUNCTION that always return val no matter the arguments"
    (import  funcy  [identity])      #_ "identity(n) -> n"

    (require hyrule [->])
    (require hyrule [->>])
    (require hyrule [as->])
    (require hyrule [doto])         #_ "| mutating "

    ; (import  funcy  [curry])
    ; (import  funcy  [autocurry])
    (import  funcy  [partial])      #_ "| applicator"
    (import  funcy  [rpartial])     #_ "| applicator"
    (require fptk._macros [p:])     #_ "| aplicator, pipe of partials"


    (import  funcy  [compose])      #_ "compose(f1, f2, ..., fn) = f1(f2(..fn(***))) | applicator"
    (import  funcy  [rcompose])     #_ "rcompose(f1, f2, ..., fn) = fn(..(f2(f1(***)))) | applicator"

    (import  funcy  [ljuxt])        #_ "ljuxt(*fs) = [f1, f2, ...](***) | applicator" ;;

    #_ "pflip(f, a) | applicator for function f(a,b) of 2 args; example: pflip(div, 4)(1) == 0.25"
    (defn pflip
        [f a]
        " creates partial applicator for f(a,b) with args a and b flipped;
          example usage: pflip(div, 4)(1) == div(1, 4) == 0.25
        "
        (fn [%x] (f %x a)))

    #_ "flip(f, a, b) = f(b, a) | calls f with flipped args"
    (defn flip [f a b] "flip(f, a, b) = f(b, a)" (f b a))

; _____________________________________________________________________________/ }}}1
; [GROUP] FP: threading ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (require fptk._macros [fm])       #_ "(fm (* it 3)) | anonymous function that accepts args in form of 'it' or '%1', '%2', ... '%9'"
    (require fptk._macros [f>])       #_ "(f> (* %1 %2) 3 4) | calculate anonymous function (with fm-syntax)"

    (comment "py | base | zip | zip(*iterables) -> zip object |")
    
    #_ "lzip(*iterables) -> List | literally just list(zip(*iterables))"
    (defn lzip [#* iterables] (list (zip #* iterables)))


    (comment "py | base | map | map(func, *iterables) -> map object |")
    (import funcy     [lmap])       #_ "lmap(f, *seqs) -> List | list version of map"

    (require fptk._macros [mapm])     #_ "| same as map, but expects fm-syntax for func"
    (require fptk._macros [lmapm])    #_ "| same as lmap, but expects fm-syntax for func"

    (import itertools [starmap])    #_ "starmap(function, iterable)" ;;

    #_ "lstarmap(function, iterable) -> list | list version of starmap"
    (defn lstarmap [function iterable]
        "literally just list(starmap(function, iterable))"
        (list (starmap function iterable)))

    (import functools [reduce])             #_ "reduce(function, sequence[, initial]) -> value | theory: reduce + monoid = binary-function for free becomes n-arg-function"
    (import funcy     [reductions])         #_ "reductions(f, seq [, acc]) -> generator | returns sequence of intermetidate values of reduce(f, seq, acc)"
    (import funcy     [lreductions])        #_ "lreductions(f, seq [, acc]) -> list | list version of reductions"
    (import funcy     [sums])               #_ "sums(seq [, acc]) -> generator | reductions with addition function"
    (import funcy     [lsums])              #_ "lsums(seq [, acc]) -> list | list version of sums"
    (import math      [prod :as product])   #_ "product(iterable, /, *, start=1) | product([2, 3, 5]) = 30"

; _____________________________________________________________________________/ }}}1
; [GROUP] FP: n-applicators ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (require hyrule [do_n])     #_ "(do_n   n #* body) -> None | expands to ~ (do body body body ...)"
    (require hyrule [list_n])   #_ "(list_n n #* body) -> List |"

    #_ "nested(n, f) | applicator f(...(f(***)))"
    (defn nested [n f]
        " constructs function f(f(f(...f))), where nesting is n times deep "
        (compose #* (list_n n f)))

    #_ "apply_n(n, f, *args, **kwargs) | f(f(f(...f(*args, **kwargs))"
    (defn apply_n [n f #* args #** kwargs]
        " applies f to args and kwargs,
          than applies f to result of prev application,
          and this is repeated in total for n times,

          n=1 is simply f(args, kwargs)
        "
        ((compose #* (list_n n f)) #* args #** kwargs))

; _____________________________________________________________________________/ }}}1

