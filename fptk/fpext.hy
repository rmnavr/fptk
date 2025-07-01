
; IMPORTS:

; Import: Modules (General, Typing) ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import sys)
    (sys.stdout.reconfigure :encoding "utf-8")

    (import ; Basic:
            math
            operator
            random
            pprint [pprint]
            hyrule
            re
            ;
            itertools
            functools
            ;
            dataclasses     [dataclass]
            enum            [Enum]
            abc             [ABC abstractmethod]
            typing          [List Tuple TypedDict Dict Union Generator Any Optional Callable Literal Type]
            ; 3rd party:
            funcy
            pydantic        [BaseModel StrictInt StrictStr StrictFloat validate_arguments]
            returns.result  [Result Success Failure])

    (setv StrictNumber (get Union #(StrictInt StrictFloat)))

; _____________________________________________________________________________/ }}}1
; Import: Functions ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (require hyrule   [ of
                        as-> -> ->> doto
                        case branch unless lif
                        ncut
                        do_n list_n ])

    ; Compositions:
    (import hyrule    [ constantly ]
            funcy     [ identity curry partial rpartial autocurry compose rcompose ljuxt ])

    ; APL:
    (import lenses    [ lens ]
            functools [ reduce ]  ; THEORY: reduce + monoid = 2-arg-function for free becomes n-arg-function
            itertools [ starmap takewhile dropwhile cycle ]
            hyrule    [ thru assoc flatten distinct ]
            funcy     [ lmap lfilter lmapcat first second last nth  ; (nth "key" dict1) — works
                        pluck lpluck pluck_attr lpluck_attr group_by partition_by count_by ])

    ; Math:
    (import hyrule    [ inc dec sign ]
            math      [ pi sin cos tan degrees radians acos asin atan atan2
                        sqrt dist hypot log exp log10 prod :as product ]
            operator  [ neg truediv :as div ])

    ; Logic/Checks:
    (import hyrule    [ xor ]
            operator  [ eq ne :as neq ]
            funcy     [ even odd isnone notnone ])

    ; Regex:
    (import funcy     [ re_find re_test re_all re_finder re_tester ])
    (import re        [ sub :as re_sub split :as re_split ])

    ; Random:
    (import random    [ choice randint uniform :as randfloat random :as rand01 ]) ; shuffle — is mutating

; _____________________________________________________________________________/ }}}1

; MY FUNCS:

; Getters ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; pluck      (funcy /function)
    ; pluck_attr (funcy /function)
    ; pluckm     (pups  /macro)

    ; get        (hy    /macro)
    ; cut        (hy    /macro)
    ; slice      (hy    /function)
    ; ncut       (hy    /macro)
    ; nth        (funcy /function)
    ;
    ; assoc      (hyrule/function)

    ; funcy /first
    ; funcy /second
    (defn third      [xs] (if (<= (len xs) 2) (return None) (return (get xs 2))))
    (defn fourth     [xs] (if (<= (len xs) 3) (return None) (return (get xs 3))))
    ; funcy /last
    (defn beforelast [xs] (if (<= (len xs) 1) (return None) (return (get xs -2))))

    (defn rest       [xs] (get xs (slice 1 None)))  ; [_ 2 3 4 5]  
    (defn butlast    [xs] (get xs (slice None -1))) ; [1 2 3 4 _]  
    (defn drop       [n xs] (if (>= n 0) (cut xs n None) (cut xs None n)))              ; drops from start/end of the list
    (defn take       [n xs] (if (>= n 0) (cut xs None n) (cut xs (+ (len xs) n) None))) ; takes from start/end of the list
    (defn pick       [ns xs] (lfor &n ns (get xs &n)))

; _____________________________________________________________________________/ }}}1
; APL ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn fltr1st [f xs] (next (gfor &x xs :if (f &x) &x) None))

    (defn lzip [#* args] (list (zip #* args)))
    (defn lstarmap [f xs] (list (starmap f xs)))
    (defn lreversed [xs] (list (reversed xs)))

    ; list methods:
    (defn count_occurrences [elem container] (container.count elem))

; _____________________________________________________________________________/ }}}1
; Compositions ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; do_n      -> returns none
    ; list_n    -> returns ...
    (defn apply_n [f n arg] ((compose #* (list_n n f)) arg))

    (defn flip [f a b] (f b a))                   ; (flip lmap [1 2 3] sqrt)
    (defn pflip [f a] (fn [%x] (f %x a)))         ; (lmap (pflip div 0.1) (thru 1 3))

; _____________________________________________________________________________/ }}}1
; Math, +/* synonims ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn normalize [v0]
        (setv norm (hypot #* v0))
        (if (!= norm 0) (return (lmap (pflip div norm) v0)) (return v0)))

    (defn half       [x] (/ x 2))
    (defn double     [x] (* x 2))
    (defn reciprocal [x] (/ 1 x))

    (defn ln      [x] (log x))              ; so, always with base = math.e
    (defn minus   [x y] (- x y))

    (defn mul     [#* args] (* #* args))    ; just a synonim for * (but a function, not macros)
    (defn smul    [#* args] (* #* args))    ; underlines usage for strings
    (defn lmul    [#* args] (* #* args))    ; underlines usage for lists
    (defn plus    [#* args] (+ #* args))    ; just a synonim for + (but a function, not macros)
    (defn sconcat [#* args] (+ #* args))    ; underlines usage for strings
    (defn lconcat [#* args] (if (= (len args) 1) (first args) (+ #* args)))    ; underlines usage for lists

; _____________________________________________________________________________/ }}}1
; Logic, Checks (Q) ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn not_ [f #* args #** kwargs] (not (f #* args #** kwargs)))

    (defn zeroQ     [x] (= x 0))
    (defn negativeQ [x] (< x 0))
    (defn positiveQ [x] (> x 0))

; _____________________________________________________________________________/ }}}1
; Strings ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn str_join [seq [sep ""]]           ; str_join(seq, *sep="")
        (if (bool sep)
            (funcy.str_join sep seq)
            (funcy.str_join seq)))

    (defn str_replace [string old new [count (- 1)]] (string.replace old new count))

    (defn #^ str  lowercase [#^ str string] (string.lower))
    (defn #^ bool endswith  [#^ str string #^ str ending] (string.endswith ending))
    (defn #^ str  strip     [#^ str string] (string.strip))
    (defn #^ str  lstrip    [#^ str string] (string.lstrip))
    (defn #^ str  rstrip    [#^ str string] (string.rstrip))

; _____________________________________________________________________________/ }}}1
; Regex ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; re_find(regex_patters, string, flags=0), re_test -> match, re_all -> list of all matches
    ; re_sub(regex_pattern, replacement, string, count=0, flags=0)
    ; re_split(r"\s+", text)

    ; re: match, search, findall, finditer, split, compile, fullmatch, escape

    ; non-escaped (commands):   .  ^  $  *  +  ? {2,4} [abc]      ( | )
    ; escaped (literals):      \. \^ \$ \* \+ \? \{ \} $$_bracket $_parenthesis \| \\ \' \"
    ; special:                 \d \D \w \W \s \S \b \B \n \r \f \v
    ; raw strings:             r"\d+" = "\\d+"

; _____________________________________________________________________________/ }}}1
; Utils ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn lprint [lst] (lmap print lst) (return None))

; _____________________________________________________________________________/ }}}1

; FOR BENCHMARKING:

; with_execution_time ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn #^ (of Tuple float str Any) #_ "[time1_in_s prompt f_result]"
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
        ;
        (setv t0 (_count))
        (setv _outp (f))
        (do_n (dec n) (f))
        (setv t1 (_count))
        (setv seconds (- t1 t0))
        (setv _time_1_s (/ seconds n))
        ;
        (case tUnit
            "s"  (do (setv time_n    seconds            ) (setv unit_str " s"))
            "ms" (do (setv time_n (* seconds 1000)      ) (setv unit_str "ms"))
            "us" (do (setv time_n (* seconds 1000000)   ) (setv unit_str "us"))
            "ns" (do (setv time_n (* seconds 1000000000)) (setv unit_str "ns")))
        (setv time_1 (/ time_n n))
        ;
        (setv line_01       f"/ ({msg})")
        (setv line_02_time1 f"\\ {time_1 :.3f} {unit_str}")
        (setv line_02_n     (str_replace f"average of {n :,} runs" "," "'"))
        (setv line_02_timeN f"test duration: {seconds :.3f} s")
        ;
        (setv _prompt (sconcat line_01 "\n"
                               line_02_time1 " as " line_02_n " // " line_02_timeN))
        (return [_time_1_s _prompt _outp]))

    ; (print (execution_time :n 100 (fn [] (get [1 2 3] 1))))

; _____________________________________________________________________________/ }}}1


