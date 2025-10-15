
    (export :objects [ constantly
                       identity
                       curry
                       autocurry
                       partial rpartial
                       compose rcompose
                       ljuxt 
                       flip pflip
                       lmap starmap lstarmap
                       reduce reductions lreductions
                       sums lsums
                       product
                       lzip
                       nested
                       apply_n
                     ]
            :macros  [ case branch unless lif as-> -> ->> doto do_n list_n
                       p:                       ; fptk macros
                       fm f>                    ; fptk macros
                       mapm lmapm               ; fptk macros
                       filterm lfilterm         ; fptk macros
                     ])

    (require hyrule [comment])

; [GROUP] FP: Control flow ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (comment "hy | base | if   | (if check true false)          | ")
    (comment "hy | base | cond | (cond check1 do1 ... true doT) | ")

    (require hyrule [case])
    (require hyrule [branch])
    (require hyrule [unless])
    (require hyrule [lif])
    (require fptk._macros [p:]) #_ "| pipe of partials"

; _____________________________________________________________________________/ }}}1
; [GROUP] FP: Composition ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import  hyrule [constantly])    #_ "(setv answer (constantly 42)) (answer 1 :x 2) -> 42"
    (import  funcy  [identity])      #_ "identity(30) -> 30"

    (require hyrule [as->])
    (require hyrule [->])
    (require hyrule [->>])
    (require hyrule [doto])

    (import  funcy  [curry])
    (import  funcy  [autocurry])
    (import  funcy  [partial])
    (import  funcy  [rpartial])
    (import  funcy  [compose])  
    (import  funcy  [rcompose]) 

    (import  funcy  [ljuxt]) #_ "ljuxt(*fs) = [f1, f2, ...] applicator |" ;;

    #_ "flip(f, a, b) = f(b, a) | example: (flip lmap [1 2 3] sqrt)"
    (defn flip [f a b] "flip(f, a, b) = f(b, a)" (f b a))

    #_ "pflip(f, a)| partial applicator with flipped args, works like: pflip(f, a)(b) = f(b, a), example: (lmap (pflip div 0.1) (thru 1 3))"
    (defn pflip
        [f a]
        " flips arguments and partially applies,
          example: pflip(f, a)(b) = f(b, a)
        "
        (fn [%x] (f %x a)))

; _____________________________________________________________________________/ }}}1
; [GROUP] FP: threading ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (require fptk._macros [fm])       #_ "| anonymous function that accepts 'it' or '%1', '%2', ... args"
    (require fptk._macros [f>])       #_ "| same as fm, but with immediate application"
    (require fptk._macros [mapm])     #_ "| same as map, but expects fm-syntax for func"
    (require fptk._macros [lmapm])    #_ "| same as lmap, but expects fm-syntax for func"

    (comment "py | base | map | map(func, *iterables) -> map object |")
    (import funcy     [lmap])       #_ "lmap(f, *seqs) -> List |"
    (import itertools [starmap])    #_ "starmap(function, iterable)" ;;

    #_ "lstarmap(function, iterable) -> list | literally just list(starmap(function, iterable))"
    (defn lstarmap [function iterable]
        "literally just list(starmap(function, iterable))"
        (list (starmap function iterable)))

    (import functools [reduce])             #_ "reduce(function, sequence[, initial]) -> value | theory: reduce + monoid = binary-function for free becomes n-arg-function"
    (import funcy     [reductions])         #_ " reductions(f, seq [, acc]) -> generator | returns sequence of intermetidate values of reduce(f, seq, acc)"
    (import funcy     [lreductions])        #_ "lreductions(f, seq [, acc]) -> List | returns sequence of intermetidate values of reduce(f, seq, acc)"
    (import funcy     [sums])               #_ " sums(seq [, acc]) -> generator | reductions with addition function"
    (import funcy     [lsums])              #_ "lsums(seq [, acc]) -> List |"
    (import math      [prod :as product])   #_ "product(iterable, /, *, start=1) | product([2, 3, 5]) = 30"
    ;;

    (comment "py | base | zip | zip(*iterables) -> zip object |")
    
    #_ "lzip(*iterables) -> List | literally just list(zip(*iterables))"
    (defn lzip [#* iterables] (list (zip #* iterables)))

; _____________________________________________________________________________/ }}}1
; [GROUP] FP: n-applicators ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (require hyrule [do_n])     #_ "(do_n   n #* body) -> None |"
    (require hyrule [list_n])   #_ "(list_n n #* body) -> List |"

    #_ "nested(n, f) | f(f(f(...f))), returns function"
    (defn nested [n f]
        " f(f(f(...f))), where nesting is n times deep,
          returns function
        "
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

