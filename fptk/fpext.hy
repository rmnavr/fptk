
    (require hyrule [comment])

    ; MAIN IMPORTS/DEFINITIONS:

; [GROUP] Import Full Modules ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import sys)
    (sys.stdout.reconfigure :encoding "utf-8")

    (import math)
    (import operator)
    (import random)
    (import re)
    (import itertools)
    (import functools)
    (import pprint [pprint])

    (import hyrule)
    (import funcy)

; _____________________________________________________________________________/ }}}1
; [GROUP] Typing ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (require hyrule [of])

    (import dataclasses [dataclass])
    (import enum        [Enum])
    (import abc         [ABC])
    (import abc         [abstractmethod])
    (import typing      [List])
    (import typing      [Tuple])
    (import typing      [TypedDict])
    (import typing      [Dict])
    (import typing      [Union])
    (import typing      [Generator])
    (import typing      [Any])
    (import typing      [Optional])
    (import typing      [Callable])
    (import typing      [Literal])
    (import typing      [Type])

    (import pydantic    [BaseModel])
    (import pydantic    [StrictInt])
    (import pydantic    [StrictStr])
    (import pydantic    [StrictFloat])
    (import pydantic    [validate_arguments :as validate_args]) ;;

    #_ "Int or Float"
    (setv StrictNumber (get Union #(StrictInt StrictFloat)))

    (import returns.result  [Result])
    (import returns.result  [Success])
    (import returns.result  [Failure])

    (import funcy [isnone])
    (import funcy [notnone])

; _____________________________________________________________________________/ }}}1
; [GROUP] Getters ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import  lenses [lens])

    (import  funcy  [first])
    (import  funcy  [second])
    (import  funcy  [last])
    (import  funcy  [nth])          #_ "nth(n, xs) | works also with dicts; zero-based index btw"
    (import  funcy  [lpluck])       #_ "lpluck(key, xs) | works also with dicts"
    (import  funcy  [lpluck_attr])

    (comment "hyrule | macro | get | get(xs,n) | ")
    (import  hyrule [ assoc ])
    (require hyrule [ ncut ])

    #_ "third(xs) -> Optional elem |"
    (defn third      [xs] (if (<= (len xs) 2) (return None) (return (get xs 2))))

    #_ "third(xs) -> Optional elem |"
    (defn fourth     [xs] (if (<= (len xs) 3) (return None) (return (get xs 3))))

    #_ "beforelast(xs) -> Optional elem |"
    (defn beforelast [xs] (if (<= (len xs) 1) (return None) (return (get xs -2))))

    #_ "rest(xs) -> List | [xx 2 3 4 5]"
    (defn rest       [xs] (get xs (slice 1 None)))

    #_ "rest(xs) -> List | [1 2 3 4 xx]"
    (defn butlast    [xs] (get xs (slice None -1)))

    #_ "drop(n, xs) -> List | drops from start/end of the list"
    (defn drop       [n xs] (if (>= n 0) (cut xs n None) (cut xs None n)))

    #_ "take(n, xs) -> List | takes from start/end of the list"
    (defn take       [n xs] (if (>= n 0) (cut xs None n) (cut xs (+ (len xs) n) None)))

    #_ "pick(ns, xs) -> List | throws error if idx doesn't exist; also works with dicts keys"
    (defn pick       [ns xs] (lfor &n ns (get xs &n)))

; _____________________________________________________________________________/ }}}1
; [GROUP] APL ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (comment "hy | base | if   | (if check true false)          | ")
    (comment "hy | base | cond | (cond check1 do1 ... true doT) | ")

    (require hyrule [case])
    (require hyrule [branch])
    (require hyrule [unless])
    (require hyrule [lif])

    (import functools [reduce])  #_ "THEORY: reduce + monoid = 2-arg-function for free becomes n-arg-function"
    (import itertools [starmap])
    (import funcy     [lmap]) ;;

    #_ "lstarmap(f,xs) -> List |"
    (defn lstarmap [f xs] (list (starmap f xs)))

    (comment "py | base | filter | filter(f, xs) -> List | ")
    (import funcy     [lfilter])

    #_ "fltr1st(f, xs) -> Optional elem | returns first found element"
    (defn fltr1st [f xs] (next (gfor &x xs :if (f &x) &x) None))

    #_ "count_occurrences(elem, xs) -> int |"
    (defn count_occurrences [elem container] (container.count elem))

    (import itertools  [cycle])
    (import hyrule     [thru])

    (import hyrule     [flatten]) #_ "| flattens to the bottom" ;;

    (defn lprint [lst] (lmap print lst) (return None))

    #_ "lzip(*args) = list(zip(*args)) |"
    (defn lzip [#* args] (list (zip #* args)))

    #_ "lreversed(*args) = list(reversed(*args)) |"
    (defn lreversed [xs] (list (reversed xs)))

; _____________________________________________________________________________/ }}}1
; [GROUP] Compositions ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import hyrule [constantly])
    (import funcy  [identity])

    (require hyrule [do_n])
    (require hyrule [list_n]) ;;

    #_ "apply_n(f, n, arg) | f(f(f(...f(arg))"
    (defn apply_n [f n arg] ((compose #* (list_n n f)) arg))

    #_ "ljuxt(f,g,...) = [f, g] applicator"
    (import funcy [ljuxt])

    (require hyrule [as->])
    (require hyrule [->])
    (require hyrule [->>])
    (require hyrule [doto])

    (import funcy   [curry])
    (import funcy   [autocurry])
    (import funcy   [partial])
    (import funcy   [rpartial])
    (import funcy   [compose])
    (import funcy   [rcompose]) ;;

    #_ "flip(f, a, b) = f(b, a) | example: (flip lmap [1 2 3] sqrt)"
    (defn flip [f a b] (f b a))

    #_ "pflip(f, a) = f(_, a) partial applicator | example: (lmap (pflip div 0.1) (thru 1 3))"
    (defn pflip [f a] (fn [%x] (f %x a)))

; _____________________________________________________________________________/ }}}1

; [GROUP] General Math ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import hyrule    [inc])
    (import hyrule    [dec])
    (import hyrule    [sign])
    (import operator  [neg])

    (defn half       [x] (/ x 2))
    (defn double     [x] (* x 2))

    #_ "reciprocal(x) = 1/x literally |"
    (defn reciprocal [x] (/ 1 x))

    (import math [sqrt])
    (import math [dist])    #_ "dist(v1, v2) -> float | ≈ √((v1x-v2x)² + (v1y-v2y)² ...)"
    (import math [hypot])   #_ "hypot(x, y, ...) | = √(x² + y² + ...)" ;;

    #_ "normalize(vector) -> vector | returns same vector if it's norm=0"
    (defn normalize [v0]
        (setv norm (hypot #* v0))
        (if (!= norm 0) (return (lmap (pflip div norm) v0)) (return v0)))

    (import operator [truediv :as div])
    (import math     [prod :as product])
    (import math     [log]) #_ "log(x, base=math.e)"
    (import math     [exp])
    (import math     [log10]) ;;

    #_ "ln(x) = math.log(x, math.e) | coexists with log for clarity"
    (defn ln [x] (log x))



; _____________________________________________________________________________/ }}}1
; [GROUP] Trigonometry ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import math [pi])
    (import math [sin])
    (import math [cos])
    (import math [tan])
    (import math [degrees])
    (import math [radians])
    (import math [acos])
    (import math [asin])
    (import math [atan])
    (import math [atan2])
    (import math [sin])

; _____________________________________________________________________________/ }}}1
; [GROUP] Funcs from base operators ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    #_ "minus(x, y) = x - y"
    (defn minus   [x y] (- x y))

    ;; *

    #_ "mul(*args) = arg1 * arg2 * ... | rename of * operator, underlines usage for numbers"
    (defn mul     [#* args] (* #* args))

    #_ "lmul(*args) = arg1 * arg2 * ... | rename of * operator, underlines usage for strings"
    (defn lmul    [#* args] (* #* args))

    #_ "smul(*args) = arg1 * arg2 * ... | rename of * operator, underlines usage for lists"
    (defn smul    [#* args] (* #* args))

    ;; +

    #_ "plus(*args) = arg1 + arg2 + ... | rename of + operator, underlines usage for numbers"
    (defn plus    [#* args] (+ #* args))

    #_ "sconcat(*args) = arg1 + arg2 + ... | rename of + operator, underlines usage for strings"
    (defn sconcat [#* args] (+ #* args))

    #_ "lconcat(*args) = arg1 + arg2 + ... | rename of + operator, underlines usage for lists"
    (defn lconcat [#* args] (if (= (len args) 1) (first args) (+ #* args)))

; _____________________________________________________________________________/ }}}1
; [GROUP] Logic and ChecksQ ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import hyrule   [xor])
    (import operator [eq])          #_ "equal"
    (import operator [ne :as neq])  #_ "non-equal"
    (import funcy    [even])
    (import funcy    [odd]) ;;

    #_ "not_(f, *args, **kwargs) = not(f(*args, **kwargs)) | "
    (defn not_ [f #* args #** kwargs] (not (f #* args #** kwargs)))

    #_ "checks directly via (= x 0)"
    (defn zeroQ     [x] (= x 0))

    #_ "checks directly via (< x 0)"
    (defn negativeQ [x] (< x 0))

    #_ "checks directly via (> x 0)"
    (defn positiveQ [x] (> x 0))

    #_ "checks directly if (= (len xs) 0)"
    (defn emptyQ    [xs] (= (len xs) 0))

; _____________________________________________________________________________/ }}}1

; [GROUP] Strings ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    #_ "str_join(seq, sep='') | rearrangement of funcy.str_join"
    (defn str_join [seq [sep ""]]
        (if (bool sep)
            (funcy.str_join sep seq)
            (funcy.str_join seq)))

    #_ "str_replace(string, old, new, count=-1) ; rename of string.replace method"
    (defn str_replace [string old new [count -1]] (string.replace old new count))

    (defn #^ str  lowercase [#^ str string] (string.lower))

    #_ "endswith(string, ending) -> bool ; rename of string.endswith method"
    (defn #^ bool endswith  [#^ str string #^ str ending] (string.endswith ending))

    (defn #^ str  strip     [#^ str string] (string.strip))
    (defn #^ str  lstrip    [#^ str string] (string.lstrip))
    (defn #^ str  rstrip    [#^ str string] (string.rstrip))

; _____________________________________________________________________________/ }}}1
; [GROUP] Regex ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ;; re: match, search, findall, finditer, split, compile, fullmatch, escape

    ;; non-escaped (commands):   .  ^  $  *  +  ? {2,4} [abc]      ( | )
    ;; escaped (literals):      \. \^ \$ \* \+ \? \{ \} $$_bracket $_parenthesis \| \\ \' \"
    ;; special:                 \d \D \w \W \s \S \b \B \n \r \f \v
    ;; raw strings:             r"\d+" = "\\d+"

    (import re        [sub :as re_sub])         #_ "re_sub(rpattern, replacement, string, count=0, flags=0) |"
    (import re        [split :as re_split])     #_ "re_split(rpattern, string) |"
    (import funcy     [re_find])                #_ "re_find(rpattern, string, flags=0) -> str| returns first found"
    (import funcy     [re_test])                #_ "re_test(rpattern, string, ...) -> bool | tests string has match (not neccessarily whole string)"
    (import funcy     [re_all])                 #_ "re_all(rpattern, string, ...) -> List |"

; _____________________________________________________________________________/ }}}1
; [GROUP] Random ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import random    [choice])    #_ "choice(xs) -> Elem | throws error for empty list"
    (import random    [randint])   #_ "randint(a, b) -> int | returns random integer in range [a, b] including both end points" 
    (import random    [uniform :as randfloat])  #_ "randfloat(a, b) -> float | range is [a, b) or [a, b] depending on rounding"
    (import random    [random :as rand01])  #_ "rand01() -> float in interval [0, 1)"

    ;; shuffle — is mutating

; _____________________________________________________________________________/ }}}1

    ; FOR BENCHMARKING:

; [GROUP] with_execution_time ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    #_ "wet(f, n=1, tUnit='ns', msg='') -> time_of_1_exec_in_seconds, pretty_string, f_result | result is from 1st function execution"
    (defn #^ (of Tuple float str Any)
        with_execution_time
        [ #^ Callable f
          *
          #^ int      [n     1]
          #^ str      [tUnit "ns"]      #_ "s/ms/us/ns"
          #^ str      [msg   "..benchmark.."]
        ]
        "returns tuple: 1) str of test result 2) function execution result"
        (setv _count hy.I.time.perf_counter)
        (setv n (int n))
        ;;
        (setv t0 (_count))
        (setv _outp (f))
        (do_n (dec n) (f))
        (setv t1 (_count))
        (setv seconds (- t1 t0))
        (setv _time_1_s (/ seconds n))
        ;;
        (case tUnit
            "s"  (do (setv time_n    seconds            ) (setv unit_str " s"))
            "ms" (do (setv time_n (* seconds 1000)      ) (setv unit_str "ms"))
            "us" (do (setv time_n (* seconds 1000000)   ) (setv unit_str "us"))
            "ns" (do (setv time_n (* seconds 1000000000)) (setv unit_str "ns")))
        (setv time_1 (/ time_n n))
        ;;
        (setv line_01       f"/ ({msg})")
        (setv line_02_time1 f"\\ {time_1 :.3f} {unit_str}")
        (setv line_02_n     (str_replace f"average of {n :,} runs" "," "'"))
        (setv line_02_timeN f"test duration: {seconds :.3f} s")
        ;;
        (setv _prompt (sconcat line_01 "\n"
                               line_02_time1 " as " line_02_n " // " line_02_timeN))
        (return [_time_1_s _prompt _outp]))

    ;; (print (execution_time :n 100 (fn [] (get [1 2 3] 1))))

; _____________________________________________________________________________/ }}}1

