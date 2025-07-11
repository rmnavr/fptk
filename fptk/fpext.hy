
    ; DOC SYNTAX: map(f, *, y, *xs) = (map f * y #* xs)
    (require hyrule [comment])

; [GROUP] Import Full Modules ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import sys)                #_ "py base module"
    (import os)                 #_ "py base module"

    (sys.stdout.reconfigure :encoding "utf-8")

    (import math)               #_ "py base module"
    (import operator)           #_ "py base module"
    (import random)             #_ "py base module"
    (import re)                 #_ "py base module"
    (import itertools)          #_ "py base module"
    (import functools)          #_ "py base module"
    (import pprint [pprint])    

    (import hyrule)             #_ "hy base module"
    (import funcy)              #_ "3rd party"

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
    (import pydantic    [validate_arguments :as validate_args]) #_ "decorator for type-checking function arguments (but not return type)"

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

    ;; dub basics:

        (comment "hy     | macro | .     | (. xs [n1] [n2] ...) -> xs[n1][n2]... | throws error when not found")
        (comment "hy     | macro | get   | (get xs n #* keys) -> xs[n][key1]... | throws error when not found")
        (import funcy [nth])        #_ "nth(n, xs) -> Optional elem | 0-based index; works also with dicts"
        (comment "py     | base  | slice | (slice start end step) | ")
        (comment "hy     | macro | cut   | (cut xs start end step) -> (get xs (slice start end step)) -> List | gives empty list when none found")
        (import  hyrule [ assoc ])  #_ "(assoc xs k1 v1 k2 v2) -> (setv (get xs k1) v1 (get xs k2) v2) -> None | also possible: (assoc xs :x 1)"
        (require hyrule [ ncut ])   

    ;; one elem getters:

        (import funcy [first])      #_ "first(xs) -> Optional elem |"
        (import funcy [second])     #_ "second(xs) -> Optional elem |" ;;

        #_ "third(xs) -> Optional elem |"
        (defn third      [xs] (if (<= (len xs) 2) (return None) (return (get xs 2))))

        #_ "fourth(xs) -> Optional elem |"
        (defn fourth     [xs] (if (<= (len xs) 3) (return None) (return (get xs 3))))

        #_ "beforelast(xs) -> Optional elem |"
        (defn beforelast [xs] (if (<= (len xs) 1) (return None) (return (get xs -2))))

        (import funcy [last])       #_ "last(xs) -> Optional elem" 

    ;; list getters:

        #_ "rest(xs) -> List | drops 1st elem of list"
        (defn rest       [xs] (get xs (slice 1 None)))

        #_ "rest(xs) -> List | drops last elem of list"
        (defn butlast    [xs] (get xs (slice None -1)))

        #_ "drop(n, xs) -> List | drops from start/end of the list"
        (defn drop       [n xs] (if (>= n 0) (cut xs n None) (cut xs None n)))

        #_ "take(n, xs) -> List | takes from start/end of the list"
        (defn take       [n xs] (if (>= n 0) (cut xs None n) (cut xs (+ (len xs) n) None)))

        #_ "pick(ns, xs) -> List | throws error if idx doesn't exist; also works with dicts keys"
        (defn pick       [ns xs] (lfor &n ns (get xs &n)))

        (import  funcy  [lpluck])       #_ "lpluck(key, xs) | works also with dicts"
        (import  funcy  [lpluck_attr])  #_ "lpluck(attr_str, xs) |" ;;

; _____________________________________________________________________________/ }}}1
; [GROUP] index-1-based getters ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import hyrule [thru :as range_])      #_ "range_(start, end, step) -> List | same as range, but with 1-based index"

    #_ "get_(xs, n) -> elem | same as get, but with 1-based index (will throw error for n=0)"
    (defn get_ [xs n]
        (when (dictQ xs) (return (get xs n)))
        (when (=  n 0) (raise (Exception "n=0 can't be used with 1-based getter")))
        (when (>= n 1) (return (get xs (dec n))))
        (return (get xs n))) ;; this line covers both n<0 and n=dict_key

    #_ "nth_(n, xs) -> Optional elem | same as nth, but with 1-based index (will throw error for n=0)"
    (defn nth_ [n xs] 
        (when (dictQ xs) (return (nth n xs)))
        (when (=  n 0) (raise (Exception "n=0 can't be used with 1-based getter")))
        (when (>= n 1) (return (nth (dec n) xs)))
        (return (nth n xs))) ;; this line covers both n<0 and n=dict_key

    #_ "slice_(start, end, step) | same as slice, but with 1-based index (it doesn't understand None and 0 for start and end arguments)"
    (defn slice_
        [ start
          end
          [step None]
        ]
        (cond (>= start 1) (setv _start (dec start))
              (<  start 0) (setv _start start)
              (=  start 0) (raise (Exception "start=0 can't be used with 1-based getter"))
              True         (raise (Exception "start in 1-based getter is probably not an integer")))
        ;;
        (cond (=  end -1) (setv _end None)
              (>= end  1) (setv _end end)
              (<  end -1) (setv _end (inc end))
              (=  end  0) (raise (Exception "end=0 can't be used with 1-based getter"))
              True        (raise (Exception "end in 1-based getter is probably not an integer")))
        (return (slice _start _end step)))

    #_ "cut_(xs, start, end, step) -> List | same as cut, but with 1-based index (it doesn't understand None and 0 for start and end arguments)"
    (defn cut_ [xs start end [step None]] (get xs (slice_ start end step)))

; _____________________________________________________________________________/ }}}1

; [GROUP] Control flow ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (comment "hy | base | if   | (if check true false)          | ")
    (comment "hy | base | cond | (cond check1 do1 ... true doT) | ")

    (require hyrule [case])
    (require hyrule [branch])
    (require hyrule [unless])
    (require hyrule [lif])

; _____________________________________________________________________________/ }}}1

; [GROUP] Compositions ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import hyrule [constantly])    #_ "(setv answer (constantly 42)) (answer 1 :x 2) -> 42"
    (import funcy  [identity])      #_ "identity(30) -> 30"

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

    #_ "ljuxt(f,g,...) = [f, g] applicator |"
    (import funcy [ljuxt])

    #_ "flip(f, a, b) = f(b, a) | example: (flip lmap [1 2 3] sqrt)"
    (defn flip [f a b] (f b a))

    #_ "pflip(f, a) = f(_, a) partial applicator | example: (lmap (pflip div 0.1) (thru 1 3))"
    (defn pflip [f a] (fn [%x] (f %x a)))

; _____________________________________________________________________________/ }}}1
; [GROUP] APL: n-applicators ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (require hyrule [do_n])     #_ "(do_n   n #* body) -> None |"
    (require hyrule [list_n])   #_ "(list_n n #* body) -> List |"

    #_ "nest(n, f) | f(f(f(...f)))"
    (defn nest [n f] (compose #* (list_n n f)))

    #_ "apply_n(n, f, *args, **kwargs) | f(f(f(...f(*args, **kwargs))"
    (defn apply_n [n f #* args #** kwargs] ((compose #* (list_n n f)) #* args #** kwargs))

; _____________________________________________________________________________/ }}}1
; [GROUP] APL: Threading ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (comment "py | base | map | map(f, *xss) -> iterator | ")
    (import funcy     [lmap])       #_ "lmap(f, *xss) -> List"
    (import itertools [starmap])    #_ "starmap(f, xs)" ;;

    #_ "lstarmap(f,xs) -> List |"
    (defn lstarmap [f xs] (list (starmap f xs)))

    (import functools [reduce])  #_ "reduce(f, xs[, x0]) -> value | reduce + monoid = binary-function for free becomes n-arg-function"

    (comment "py | base | zip | zip(*xss) -> iterator I guess | ")
    
    #_ "lzip(*xss) = list(zip(*xss)) |"
    (defn lzip [#* args] (list (zip #* args)))

; _____________________________________________________________________________/ }}}1
; [GROUP] APL: Filtering ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (comment "py | base | filter | filter(f or None, xs) -> filter object | when f=None, checks if elems are True")
    (import funcy [lfilter]) #_ "lfilter(f, xs) -> List"

    #_ "fltr1st(f, xs) -> Optional elem | returns first found element (or None)"
    (defn fltr1st [f xs] (next (gfor &x xs :if (f &x) &x) None))

    #_ "count_occurrences(elem, xs) -> int | rename of list.count method"
    (defn count_occurrences [elem container] (container.count elem))

; _____________________________________________________________________________/ }}}1
; [GROUP] APL: Work on lists ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import hyrule     [flatten])   #_ "flattens to the bottom" ;;

    #_ "lprint(lst) -> (lmap print lst) |"
    (defn lprint [lst] (lmap print lst) (return None))

    (comment "py | base | reversed | reversed(xs) -> iterator |") 

    #_ "lreversed(*args) = list(reversed(*args)) |"
    (defn lreversed [xs] (list (reversed xs)))

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

    (import math     [exp])

    (import math     [log]) #_ "log(x, base=math.e)" ;;

    #_ "ln(x) = math.log(x, math.e) | coexists with log for clarity"
    (defn ln [x] (log x))

    (import math     [log10])

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
    (import math [atan2]) #_ "atan2(y, x) -> value | both signs are considered"
    (import math [sin])

; _____________________________________________________________________________/ }}}1
; [GROUP] Base operators to functions ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

        #_ "minus(x, y) = x - y |"
        (defn minus [x y] (- x y))

    ;; *

        #_ "mul(*args) | multiplication as a monoid (will not give error when used with 0 or 1 args)"
        (defn mul [#* args] (reduce operator.mul args 1))

        #_ "lmul(*args) = arg1 * arg2 * ... | rename of * operator, underlines usage for list"
        (defn lmul [#* args] (* #* args))

        #_ "smul(*args) = arg1 * arg2 * ... | rename of * operator, underlines usage for string"
        (defn smul [#* args] (* #* args))

    ;; +

        #_ "plus(*args) | addition as a monoid (will not give error when used with 0 or 1 args)"
        (defn plus [#* args] (reduce operator.add args 0))

        #_ "sconcat(*args) | string concantenation as a monoid (will not give error when used with 0 or 1 args)"
        (defn sconcat [#* args] (reduce (fn [%s1 %s2] (+ %s1 %s2)) args ""))

        #_ "lconcat(*args) | list concantenation as a monoid (will not give error when used with 0 or 1 args)"
        (defn lconcat [#* args] (reduce (fn [%s1 %s2] (+ %s1 %s2)) args []))

; _____________________________________________________________________________/ }}}1
; [GROUP] Logic and ChecksQ ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import hyrule   [xor])

    (import operator [eq])              #_ "equal"
    (import operator [ne :as neq])      #_ "non-equal"
    (import funcy    [even :as evenQ])
    (import funcy    [odd  :as oddQ])   ;;

    #_ "| checks directly via (= x 0)"
    (defn zeroQ     [x] (= x 0))

    #_ "| checks directly via (< x 0)"
    (defn negativeQ [x] (< x 0))

    #_ "| checks directly via (> x 0)"
    (defn positiveQ [x] (> x 0))

    #_ "| checks literally if (= (len xs) 0)"
    (defn zerolenQ [xs] (= (len xs) 0))

    #_ "(istype tp x) -> (= (type x) tp) |"
    (defn istype [tp x] (= (type x) tp))

    (defn intQ   [x] (= (type x) int))
    (defn floatQ [x] (= (type x) float))
    (defn dictQ  [x] (= (type x) dict))

    (import funcy [is_list  :as listQ ])  #_ "listQ(seq)  | checks if seq is list"
    (import funcy [is_tuple :as tupleQ])  #_ "tupleQ(seq) | checks if seq is tuple"

    #_ "fnot(f, *args, **kwargs) = not(f(*args, **kwargs)) | "
    (defn fnot [f #* args #** kwargs] (not (f #* args #** kwargs)))

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

    ;; Theory:
    ;;      re: match, search, findall, finditer, split, compile, fullmatch, escape
    ;;      non-escaped (commands):   .  ^  $  *  +  ? {2,4} [abc]      ( | )
    ;;      escaped (literals):      \. \^ \$ \* \+ \? \{ \} $$_bracket $_parenthesis \| \\ \' \"
    ;;      special:                 \d \D \w \W \s \S \b \B \n \r \f \v
    ;;      raw strings:             r"\d+" = "\\d+"

    (import re        [sub :as re_sub])         #_ "re_sub(rpattern, replacement, string, count=0, flags=0) |"
    (import re        [split :as re_split])     #_ "re_split(rpattern, string) |"
    (import funcy     [re_find])                #_ "re_find(rpattern, string, flags=0) -> str| returns first found"
    (import funcy     [re_test])                #_ "re_test(rpattern, string, ...) -> bool | tests if string has match (not neccessarily whole string)"
    (import funcy     [re_all])                 #_ "re_all(rpattern, string, ...) -> List |"

; _____________________________________________________________________________/ }}}1
; [GROUP] Random ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import random    [choice])                 #_ "choice(xs) -> Elem | throws error for empty list"
    (import random    [randint])                #_ "randint(a, b) -> int | returns random integer in range [a, b] including both end points" 
    (import random    [uniform :as randfloat])  #_ "randfloat(a, b) -> float | range is [a, b) or [a, b] depending on rounding"
    (import random    [random :as rand01])      #_ "rand01() -> float | generates random number in interval [0, 1) "

    ;; shuffle — is mutating

; _____________________________________________________________________________/ }}}1

; [GROUP] IO ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import os.path [exists :as file_existsQ]) #_ "file_existsQ(filename) | also works on folders" ;;

    #_ "read_file(file_name, encoding='utf-8') -> str | reads whole file content "
    (defn read_file
        [ #^ str file_name
          #^ str [encoding "utf-8"]
        ]
        (with [file (open file_name "r" :encoding encoding)] (setv outp (file.read)))
        (return outp))

    #_ "write_file(text, file_name, mode='w', encoding='utf-8') | modes: 'w' - (over)write, 'a' - append, 'x' - exclusive creation"
    (defn write_file
        [ #^ str text
          #^ str file_name
          #^ str [mode "w"]
          #^ str [encoding "utf-8"]
        ]
        (with [file (open file_name mode :encoding encoding)] (file.write text)))

; _____________________________________________________________________________/ }}}1
; [GROUP] Benchmarking ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    #_ "w_e_t(f, n=1, tUnit='ns', msg='') -> avrg_time_of_1_run_in_seconds, pretty_string, f_result | f_result is from 1st function execution"
    (defn #^ (of Tuple float str Any)
        with_execution_time
        [ #^ Callable f
          *
          #^ int      [n     1]
          #^ str      [tUnit "ns"]      #_ "s/ms/us/ns"
          #^ str      [msg   ""]
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
    ;;
    ;; (print (execution_time :n 100 (fn [] (get [1 2 3] 1))))

    #_ "| (setv print_with_time (dt_printer :show_initialization_message True)) (print_with_time 1)"
    (defn dt_printer [[show_initialization_message True]]
        (setv time_getter hy.I.time.perf_counter)
        ;;
        (setv first_run True)
        (setv time_of_last_call (time_getter))
        ;;
        (defn printer [#* args]
            (nonlocal time_of_last_call)
            (nonlocal show_initialization_message)
            (nonlocal first_run)
            (when first_run (when show_initialization_message (print "[Timer initialized]"))
                            (setv first_run False)
                            (return None))
            ;;
            (setv prev_time time_of_last_call)
            (setv time_of_last_call (time_getter))
            (setv dt (- time_of_last_call prev_time))
            ;;
            (print f"[dT = {dt :.9f} s]" #* args)
            (return None))
        ;;
        (printer "first run (this line will not be printed)")
        (return printer))

; _____________________________________________________________________________/ }}}1


