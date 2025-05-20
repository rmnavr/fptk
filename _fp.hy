
; For outer usage of this lib ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; (import sys)
    ; (. sys.stdout (reconfigure :encoding "utf-8"))
    ; (sys.path.append "../HyExt")

    ; (require hyrule [of as-> -> ->> doto case branch unless lif do_n list_n ncut])
    ; (import  _fp *)
    ; (require _fp [f:: fm lns &+ &+> l> l>=] :readers [L])


; _____________________________________________________________________________/ }}}1

; IMPORTS:

; General ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (require hyrule [ of
                      as-> -> ->> doto
                      case branch unless lif
                      do_n list_n
                      ncut ])

    (import math
            operator
            random)

    (import pprint [pprint]
            re)

    (import dataclasses [dataclass]
            enum        [Enum]
            abc         [ABC abstractmethod]
            typing      [List Tuple TypedDict Dict Union Generator Any Optional Callable Literal])

    (import itertools
            functools
            funcy)

; _____________________________________________________________________________/ }}}1
; Math ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import math [ sqrt pi sin cos tan log exp dist log10
                   degrees radians acos asin atan atan2 hypot
                   prod :as product ; product(seq)
                 ])

    (import operator [ neg  
                       truediv :as div
                       ; «add» and «mul» are funcs of 2 args
                       eq
                       ne :as neq
                     ])

    (import random [ choice
                     randint
                     uniform :as randfloat
                     random  :as rand01
                     ; shuffle — is mutating
                   ])

; _____________________________________________________________________________/ }}}1
; Functional ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import itertools [starmap takewhile dropwhile cycle])
    (import functools [reduce])

    (import lenses [lens]) 

    (import hyrule [ thru assoc inc dec sign constantly xor
                     flatten    ; non-mutating btw
                     butlast    ; returns generator
                     drop_last  ; returns generator // drop_last(n, seq)
                     rest       ; returns iterator
                     distinct   ; returns generator
                   ])

    (import funcy  [ lmap lfilter lmapcat   ; returns list       
                     take                   ; returns first n elems // take(n, seq)
                     first second last nth  ; returns one elem      // nth(n, seq)
                     drop                   ; returns iterator
                     re_find re_test re_all re_finder re_tester
                     curry partial rpartial autocurry
                     compose rcompose ljuxt
                     identity   ; same as hyrule.constantly
                     lpluck pluck pluck_attr lpluck_attr ; // lpluck(i, seq) -> works on lists/dicts
                     group_by
                     ])

    ;  first
    ;  | second
    ;  | |   last
    ;  | |   |
    ; [1 2 3 4 5]
    ;  \_____/ butlast
    ;    \_____/ rest

; _____________________________________________________________________________/ }}}1

; MY FUNCS:

; Core, Math, Functional ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; core:

        (defn minus [x y] (- x y))
        (defn mul     [#* args] (* #* args))    ; just a synonim for * (but a function, not macros)
        (defn plus    [#* args] (+ #* args))    ; just a synonim for + (but a function, not macros)
        (defn sconcat [#* args] (+ #* args))    ; just a synonim for + (but a function, not macros)
        (defn lconcat [#* args] (+ #* args))    ; just a synonim for + (but a function, not macros)

        (defn lprint [lst] (lmap print lst) (return None))

    ; math/logic:

        (defn not_ [f #* args] (not (f #* args)))
        (defn zeroQ     [x] (if (= x 0) True False))
        (defn negativeQ [x] (if (< x 0) True False))
        (defn positiveQ [x] (if (> x 0) True False))

        (defn normalize [v0]
            (setv norm (hypot #* v0))
            (if (!= norm 0)
                (return (lmap (pflip div norm) v0))
                (return v0)))

    ; functional:

        (defn lzip [#* args] (list (zip #* args)))    ;
        (defn flip [f a b] (f b a))                   ; (flip lmap [1 2 3] sqrt)
        (defn pflip [f a] (fn [%x] (f %x a)))         ; (lmap (pflip div 0.1) (thru 1 3))

        (defn fltr1st [predicate iterable]
            (next (gfor x iterable :if (predicate x) x) None))

        (defn asListable [f lst] (f #* lst))          ; (asListable plus [1 2 3])
        (defn asVariadic [f #* args] (f [#* args]))   ; (asVariadic sum 1 2 3)

; _____________________________________________________________________________/ }}}1
; Strings ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn str_join [seq [sep ""]]           ; str_join(seq, *sep="")
        (if (bool sep)
            (funcy.str_join sep seq)
            (funcy.str_join seq)))

    (defn str_replace [string old new [count (- 1)]]
        (string.replace old new count))

    (defn #^ str  lowercase [#^ str string] (string.lower))
    (defn #^ bool endswith  [#^ str string #^ str ending] (string.endswith ending))
    (defn #^ str  strip     [#^ str string] (string.strip))

; _____________________________________________________________________________/ }}}1
; for Benchmarking ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1
    
    (defn execution_time
        [ #^ Callable f
          *
          #^ int      [n 1]
          #^ str      [tUnit "ns"]      #_ "s/ms/us/ns"
          #^ str      [msg "..benchmark.."]
        ]
        (setv _count hy.I.time.perf_counter)
        (setv n (int n))
        ;
        (setv t0 (_count))
        (do_n n (f))
        (setv t1 (_count))
        (setv seconds (- t1 t0))
        ;
        (case tUnit
            "s"  (do (setv time_n   seconds)
                     (setv unit_str " s"))
            "ms" (do (setv time_n   (* seconds 1000))
                     (setv unit_str "ms"))
            "us" (do (setv time_n   (* seconds 1000000))
                     (setv unit_str "us"))
            "ns" (do (setv time_n   (* seconds 1000000000))
                     (setv unit_str "ns")))
        (setv time_1 (/ time_n n))
        ;
        (setv line_01       f"/ ({msg})")
        (setv line_02_time1 f"\\ {time_1 :.3f} {unit_str}")
        (setv line_02_n     (str_replace f"average of {n :,} runs" "," "'"))
        (setv line_02_timeN f"test duration: {seconds :.3f} s")
        ;
        (sconcat line_01 "\n" 
                 line_02_time1 " as " line_02_n " // " line_02_timeN))

    ; (print (execution_time :n 100 (fn [] (get [1 2 3] 1))))

; _____________________________________________________________________________/ }}}1

; MACROS:

; [testing setup] ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv _test_macro_FAnnot    False)
    (setv _test_macro_FM        False)
    (setv _test_macroses_Lens   False)
    (setv _test_macro_Pluckm    False)

    (defn _test_lines
        [ #^ bool testQ
          #^ str  msg
          #* lines
        ]
        (when testQ
            (print "==" msg "==")
            (lmap (fn [%x] (print ">" %x)) lines)
            (print "")))

; _____________________________________________________________________________/ }}}1

; f:: (for readable functions annotations) ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defmacro f:: [#* macro_args]
        ;
        (import hyrule [butlast])
        ;
        (setv fInputsOutputs (get macro_args (slice None None 2)))
        (setv fInputs (get fInputsOutputs (slice 0 (- 1))))
        (setv fOutput (get fInputsOutputs (- 1)))
        `(of Callable ~fInputs ~fOutput))

    (_test_lines _test_macro_FAnnot "f::"
        (setx #_ DC annotTest (f:: int -> (of List int) -> (of Dict str int) -> (of Optional int) -> (f:: int -> int))) 
        (setx #_ DC annotVariadic (f:: int -> ... -> float))
    )

; _____________________________________________________________________________/ }}}1
; #L  (rename of #% macros) — does NOT work in REPL ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defreader L
      (import hyrule [flatten inc])
      (setv expr (.parse-one-form &reader))
      (setv %symbols (sfor a (flatten [expr])
                           :if (and (isinstance a hy.models.Symbol)
                                    (.startswith a '%))
                           (-> a
                               (.split "." :maxsplit 1)
                               (get 0)
                               (cut 1 None))))
      `(fn [;; generate all %i symbols up to the maximum found in expr
            ~@(gfor i (range 1 (-> (lfor a %symbols
                                         :if (.isdigit a)
                                         (int a))
                                   (or #(0))
                                   max
                                   inc))
                    (hy.models.Symbol (+ "%" (str i))))
            ;; generate the #* parameter only if '%* is present in expr
            ~@(when (in "*" %symbols)
                    '(#* %*))
            ;; similarly for #** and %**
            ~@(when (in "**" %symbols)
                    '(#** %**))]
         ~expr))

    ; #L((abs %1) (- 3))

; _____________________________________________________________________________/ }}}1
; fm  (defmacro similar to #L macro) — works in REPL ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; recognizes %1..%9 as arguments

    (defmacro fm [expr]
        (import hyrule [flatten thru])
        ;
        (setv models (flatten expr))
        (setv args (filter (fn [%x] (= (type %x) hy.models.Symbol)) models))    ; Symbols
        (setv args (filter (fn [%x] (and (= (get %x 0) "%")                     ; "%_"
                                         (= (len %x) 2)))
                                    args))
        (setv args (filter (fn [%x] (and (.isdigit (get %x 1))                  ; "%1..%9"
                                         (!= "0" (get %x 1))))
                                    args))               
        (setv args (sorted args))
        (if (= (len args) 0)
            (setv maxN 0)
            (setv maxN (int (get args (- 1) (- 1)))))
        ;
        (setv inputs (lfor n (thru 1 maxN) (hy.models.Symbol f"%{n}")))
        ; (print (hy.repr `(fn [~@inputs] ~expr)))
        (return `(fn [~@inputs] ~expr)))

    (_test_lines _test_macro_FM "fm"
        ((fm 3))
        ((fm %1) 3)
        ((fm %2) 1 3)
        ((fm (abs 3)))
        ((fm (abs %1)) (- 3))
        ((fm (abs %2)) 1 (- 3))
    )

; _____________________________________________________________________________/ }}}1
; lens: lns, &+, &+>, l>, l>= ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

; ■ Macro: lns (upgrades lens arg syntax) ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    (defmacro lns [#* macro_args]
        (import  hyrule [rest])
        ;
        (setv args (list macro_args)) ; for mutations
        (for [[&i &arg] (enumerate args)]
            (cond ; Integer/String/Symbol
                  (or (= (type &arg) hy.models.Integer)  ; 1 -> [1]
                      (= (type &arg) hy.models.String)   ; "str" -> ["str"]
                      (= (type &arg) hy.models.Symbol))  ; vrbl -> [vrbl]
                  (setv (get args &i) [&arg]) 
                  ; Keyword: :ololo -> ["ololo"]
                  (= (type &arg) hy.models.Keyword)
                  (setv (get args &i) [(get (str &arg) (slice 1 None))]) ; (str :x) will give ":x", this is why slice is required
                  ; (-1) -> [(- 1)]
                  (and (= (type &arg) hy.models.Expression)
                       (= (get &arg 0) (hy.models.Symbol "-"))
                       (= (len &arg) 2)
                       (= (type (get &arg 1)) hy.models.Integer))
                  (setv (get args &i) [&arg])
                  ; .pups -> pups
                  (and (= (type &arg) hy.models.Expression)
                       (= (get &arg 0) (hy.models.Symbol "."))
                       (= (get &arg 1) (hy.models.Symbol "None")))
                  (setv (get args &i) (get &arg 2))
                  ; (mth> f 1) -> (call "f" 1)
                  (and (= (type &arg) hy.models.Expression)
                       (= (get &arg 0) (hy.models.Symbol "mth>")))
                  (setv (get args &i) `(call ~(str (get &arg 1)) ~@(get &arg (slice 2 None)))) 
                  ; (mut> f 1) -> (call_mut "f" 1)
                  (and (= (type &arg) hy.models.Expression)
                       (= (get &arg 0) (hy.models.Symbol "mut>")))
                  (setv (get args &i) `(call_mut ~(str (get &arg 1)) ~@(get &arg (slice 2 None))))))
        ; process (dndr> ...): // TODO: recognise (dndr> & ...) as bitwise_and
        (setv last_arg (get args (- 1)))
        (cond (and (= (type last_arg) hy.models.Expression)
                   (= (get last_arg 0) (hy.models.Symbol "dndr>")))
             `(->  (. lens ~@(get args (slice 0 (- 1))))
                  ~(get last_arg (slice 1 None)))
              (and (= (type last_arg) hy.models.Expression)
                   (= (get last_arg 0) (hy.models.Symbol "dndr>>")))
             `(->> (. lens ~@(get args (slice 0 (- 1))))
                  ~(get last_arg (slice 1 None)))
              True
             `(. lens ~@args)))

; ________________________________________________________________________/ }}}2
; ■ Macro: &+, &+>, l>, l>= ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    ; compose lens, add setters/getters

    (defmacro &+ [#* macro_args]
        (import hyrule [rest butlast])
        ;
        (setv lenses (butlast macro_args))
        (setv func   (get macro_args (- 1)))
       `(& ~@lenses (lns ~func)))

    ; compose lens, add setters/getters, apply

    (defmacro &+> [#* macro_args]
        (import  hyrule [rest butlast])
        ;
        (setv variable (get macro_args 0))
        (setv lenses   (butlast (rest macro_args)))
        (setv func     (get macro_args (- 1)))
       `((& ~@lenses (lns ~func)) ~variable))

    ; construct lens, apply:

    (defmacro l> [#* macro_args]
        (import  hyrule [rest])
        ;
        (setv variable    (get macro_args 0))
        (setv lenses_args (rest macro_args))
       `((lns ~@lenses_args) ~variable))

    (defmacro l>= [#* macro_args]
        (import  hyrule [rest])
        ;
        (setv variable    (get macro_args 0))
        (setv lenses_args (rest macro_args))
       `(&= ~variable (lns ~@lenses_args)))

; ________________________________________________________________________/ }}}2

    ; - [&] combine
    ; - [+] add getter/setter
    ; - [>] apply (requires for nice threading usage (-> macro))
    ; - [l] understands «lns» syntax

; ■ tests and doc ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    (_test_lines _test_macroses_Lens "lens macroses"
        (do (setv vrbl 3)
            (setv data  (list_n 3 [[1 2 3] [4 5 6] [7 8 9]]))
            (setv data1 [1 2 3])
            "vrbls initialized"
        )
        ;
        (lns 1                      ; -> [1]        // GetitemLens(...)
             vrbl                   ; -> [vrbl]     // GetitemLens(...) // if vrbl not defined, will give Error (this is good)
             (- 1)                  ; -> [(- 1)]    // GetitemLens(...) // only works for Integers! (- vrbl) will translate to (- vrbl), which will most likely give error
             [(- vrbl 1)]           ; no change
             "str"                  ; -> ["str"]    // GetitemLens(...)     
             :str                   ; -> ["str"]    // GetitemLens(...)
             .attr                  ; -> attr       // GetZoomAttrTraversal('attr')
             (Each))                ; no change
        (lns 1 (mth>   sort 1))     ; -> (call "f" 1)
        (lns 1 (mut>   sort 1))     ; -> (call_mut "f" 1)
        (lns 1 (dndr>  / 1))        ; -> (/ (lens) 1)
        (lns 1 (dndr>> / 1))        ; -> (/ 1 (lens))
        ;
        (lns 1 2)                                   ; define UL
        (lns 1 2 (mth> sort 1))                     ; define SF
        (&   (lns 1) (lns 2 (dndr> + 1)))           ; «&» usage #1: composition (ULs + last one can be UL/SF)
        (&   data (lns 1 2 (get)) (lns 2 (get)))    ; «&» usage #2: SFs application (one by one)
        (&+  (lns 1) (lns 2) (set "here"))          ; / compose ULs and SF ...
        (&+> data (lns 1) (lns 2) (mut> reverse))   ; \ .. and then apply
        (l>  data 1 2 (set "here"))                 ; define SF and apply
        (l>= data1 (Each) (modify math.sqrt))       ; define SF, apply, upd value
        ; same as:
        (. lens [1] [2])                                                    ; define UL
        (. lens [1] [2] (call "sort" 1))                                    ; define SF
        (& (. lens [1]) (+ (. lens [2]) 1))                                 ; «&» usage #1: composition (ULs + last one can be UL/SF)
        (& data (. lens [1] [2] (get)) (. lens [2] (get)))                  ; «&» usage #2: SFs application (one by one)
        (& (. lens [1]) (. lens [2]) (. lens (set "here")))                 ; / compose ULs and SF ...
        ((& (. lens [1]) (. lens [2]) (. lens (call_mut "reverse"))) data)  ; \ .. and then apply
        ((. lens [1] [2] (set "here")) data)                                ; define SF and apply
        (&= data1 (. lens (Each) (modify math.sqrt)))                       ; define SF, apply, upd value
        ;
        ; threading without lens composition:
        (-> data (l> 1 (get)))                  ; RECOMMENDED USAGE
        (-> data (. lens [1] (get)))            ; lens[1].(get)(data)
        (-> data (& (lns 1 (get))))
        (-> data ((lns 1 (get))))               ; requires second wrapping () because «-> macro» would parse: (lns data 1 ...)
        ; threading with lens composition:
        (-> data (&+> (lns 1) (lns 2) (get)))   ; RECOMMENDED USAGE
        (-> data (. (& (lns 1) (lns 2)) (get)))
        ; combination example:
        (&+> (list_n 3 data)
             (lns 1)
             (lns 2 (Fork (& (lns 1) (lns 2))
                          (lns 2)))
             (set "x"))
    )

; ________________________________________________________________________/ }}}2

; _____________________________________________________________________________/ }}}1
; pluckm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defmacro pluckm [indx iterable]
        (cond ; .attr
              (and (= (type indx) hy.models.Expression)
                   (= (get indx 0) (hy.models.Symbol "."))
                   (= (get indx 1) (hy.models.Symbol "None")))
              (return `(lpluck_attr ~(str (get indx 2)) ~iterable))
              ; :keyword
              (= (type indx) hy.models.Keyword)
              (return `(lpluck ~(get (str indx) (slice 1 None)) ~iterable))) 
              ; 
              True
              (return `(lpluck ~indx ~iterable)))

    (_test_lines _test_macro_Pluckm "pluckm"
        (do (setv xs [[0 1] [2 3]])
            (setv ds [(dict :x 0 :y 1) (dict :x 2 :y 3)])
            (setv i  0)
            (setv i  0)
            (defclass [dataclass] Point [] (#^ int x) (#^ int y))
            (setv ps [(Point 0 1) (Point 2 3)])
            "vrbls initialized"
        )
        ;
        (pluckm  0      xs)    ; -> (lpluck      0       xs)
        (pluckm i       xs)    ; -> (lpluck      i       xs)
        (pluckm (- 1 1) xs)    ; -> (lpluck      (- 2 1) xs)
        (pluckm :x      ds)    ; -> (lpluck      "x"     ds)
        (pluckm "x"     ds)    ; -> (lpluck      "x"     ds)
        (pluckm .x      ps)    ; -> (lpluckattr  "x"     ps)
    )

; _____________________________________________________________________________/ }}}1
    
