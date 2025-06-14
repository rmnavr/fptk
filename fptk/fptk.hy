
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

    (require hyrule   [ of as-> -> ->>
                        doto case branch unless lif
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
            funcy     [ lmap lfilter lmapcat first second last nth
                        pluck lpluck pluck_attr lpluck_attr group_by partition_by count_by ])

    ; Math:
    (import hyrule    [ inc dec sign xor ]
            math      [ pi sin cos tan degrees radians acos asin atan atan2
                        sqrt dist hypot log exp log10 prod :as product #_ "product(seq)" ]
            operator  [ neg truediv :as div ])

    ; Logic/Checks:
    (import operator  [ eq ne :as neq ]
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
    ;
    ; assoc      (hyrule/function)
    ; nth        (funcy /function)

    ; funcy /first
    ; funcy /second
    (defn third       [xs] (if (<= (len xs) 2) (return None) (return (get xs 2))))
    (defn fourth      [xs] (if (<= (len xs) 3) (return None) (return (get xs 3))))
    ; funcy /last
    (defn beforelast  [xs] (if (<= (len xs) 1) (return None) (return (get xs -2))))

    (defn rest        [xs]   (list (hyrule.rest xs)))               ; [_ 2 3 4 5]   x1
    (defn drop_firstN [n xs] (list (funcy.drop n xs)))              ; [_ _ 3 4 5]
    (defn lastN       [n xs] (get xs (slice (- (len xs) n) None)))  ; [_ _ 3 4 5]
    (defn butlast     [xs]   (list (hyrule.butlast xs)))            ; [1 2 3 4 _]   x1
    (defn drop_lastN  [n xs] (list (hyrule.drop_last n xs)))        ; [1 2 3 _ _]
    (defn firstN      [n xs] (list (funcy.take n xs)))              ; [1 2 3 _ _]

    ; ==========================================================================

    (defn pick [ns xs] (lfor &n ns (get xs &n)))

; _____________________________________________________________________________/ }}}1
; Compositions ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; do_n
    ; list_n
    (defn apply_n [f n arg] ((compose #* (list_n n f)) arg))

    (defn flip [f a b] (f b a))                   ; (flip lmap [1 2 3] sqrt)
    (defn pflip [f a] (fn [%x] (f %x a)))         ; (lmap (pflip div 0.1) (thru 1 3))

    ; maybe not needed:
    (defn asListable [f lst] (f #* lst))          ; (asListable plus [1 2 3])
    (defn asVariadic [f #* args] (f [#* args]))   ; (asVariadic sum 1 2 3)

; _____________________________________________________________________________/ }}}1
; APL ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn fltr1st [f xs] (next (gfor &x xs :if (f &x) &x) None))

    (defn lzip [#* args] (list (zip #* args)))
    (defn lstarmap [f xs] (list (starmap f xs)))
    (defn lreversed [xs] (list (reversed xs)))

    ; list methods:
    (defn count_occurrences [elem container] (container.count elem))

; _____________________________________________________________________________/ }}}1
; Math, +/* synonims ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defn normalize [v0]
        (setv norm (hypot #* v0))
        (if (!= norm 0) (return (lmap (pflip div norm) v0)) (return v0)))

    (defn half       [x] (/ x 2))
    (defn double     [x] (* x 2))
    (defn reciprocal [x] (/ 1 x))

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

    (defn zeroQ     [x] (if (= x 0) True False))
    (defn negativeQ [x] (if (< x 0) True False))
    (defn positiveQ [x] (if (> x 0) True False))

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

; MACROS:

; [helper funcs for macroses] ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

; ■ neg integer ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    ; (- 1)
	(-> (defn _isNegInteger
            [ arg
            ]
            (and (= (type arg) hy.models.Expression)
                 (= (get arg 0) (hy.models.Symbol "-"))
                 (= (len arg) 2)
                 (= (type (get arg 1)) hy.models.Integer)))
		eval_and_compile)

    ; (_isNegInteger '(- 3))

; ________________________________________________________________________/ }}}2
; ■ expr with head symbol ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    ; (head ...)
	(-> (defn _isExprWithHeadSymbol
            [ arg
              #^ str head
            ]
            (and (= (type arg) hy.models.Expression)
                 (= (get arg 0) (hy.models.Symbol head))))
		eval_and_compile)

    ; (_isExprWithHeadSymbol '(bubr 1 2 3) "bubr")

; ________________________________________________________________________/ }}}2

; ■ DOC: deconstructing dotted expr ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    (when False
        '.attr                      ; only used in macroses
        'obj.attr                   ; / access attr (no call)
        '(. obj attr)               ; \
        '(. obj (mth arg1 arg2))    ; access mth (and call))
        '(.mth obj arg1 arg2)       ; access mth (and call))

        '.attr                      ; (E [. None attr])
        'obj.attr                   ; (E [. obj attr])
        '(. obj attr)               ; (E [. obj attr])
        '(. obj (mth arg1 arg2))    ; (E [. obj (E mth arg1 arg2)])
        '(.mth obj arg1 arg2)       ; (E [. None mth] obj arg1 arg2)
    )

; ________________________________________________________________________/ }}}2
; ■ .dotted ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    ; .x
    (-> (defn _isDotted
            [ arg
            ]
            (and (= (type arg) hy.models.Expression)
                 (= (get arg 0) (hy.models.Symbol "."))
                 (= (get arg 1) (hy.models.Symbol "None"))))
        eval_and_compile)

    ; x
    (-> (defn _extractDotted
            [ arg
            ]
            (get arg 2))
        eval_and_compile)

    ; (_isDotted '.x)
    ; (_extractDotted '.x)

; ________________________________________________________________________/ }}}2
; ■ (.dottedCall obj arg1 arg2 ...) ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    ; (.mth obj 1 2)
    ;  ---- -------
    ;  head  args
    (-> (defn _isDottedCall
            [ arg
            ]
            (and (=  (type arg) hy.models.Expression)
                 (_isDotted (get arg 0))))
        eval_and_compile)

    (-> (defn _extractDottedCall
            [ arg
            ]
            (dict :head (get arg 0 2)
                  :args (cut arg 1 None)))
        eval_and_compile)


    ; (_isDottedCall '(.obj obj 1 2))
    ; (_extractDottedCall '(.obj obj 1 2))

; ________________________________________________________________________/ }}}2
; ■ (. dottedExpr obj ...) // TODO ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2



; ________________________________________________________________________/ }}}2

; ■ :attr: // NOT USED ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    ; leftover from lns macro:
    ;
    ;(_isAttrAccess &arg)
    ;(setv (get args &i) (hy.models.Symbol (_extractAttrName &arg)))

    ; leftover from pluckm macro:
    ;
    ; (_isAttrAccess indx)
    ; (return `(lpluck_attr ~(_extractAttrName indx) ~iterable)))

	(-> (defn #^ bool
            _isAttrAccess
            [ arg
            ]
            (setv arg_str (str arg))
            (and (= (type arg) hy.models.Keyword)
                 (> (len arg_str) 2)
                 (= (get arg_str (- 1)) ":")))
		eval_and_compile)

	(-> (defn #^ str
            _extractAttrName
            [ arg
            ]
            (cut (str arg) 1 (- 1)))
		eval_and_compile)

; ________________________________________________________________________/ }}}2

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


; _____________________________________________________________________________/ }}}1
; fm  (defmacro similar to #L macro) — works in REPL ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; recognizes %1..%9 as arguments
    ; nested fm calls will not work as intended

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


; _____________________________________________________________________________/ }}}1
; p>  (pipe of partials) ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defmacro p> [#* args]
        (import funcy [partial])
        (setv pargs [])
        (for [&arg args]
              (cond ; .x  -> (partial flip getattr "x")
                    (_isDotted &arg)
                    (pargs.append `(partial flip getattr ~(str (_extractDotted &arg))))
                    ; (. mth 2 3) -> ...
                    (_isDottedCall &arg)
                    (do (pargs.append `(partial flip getattr
                                            ~(str (get (_extractDottedCall &arg) "head")))) ; -> mth)
                        (pargs.append `(partial (fn [%args %mth] (%mth (unpack_iterable  %args)))
                                                [~@(get (_extractDottedCall &arg) "args")])))
                    ; abs -> (partial abs)
                    (= (type &arg) hy.models.Symbol)
                    (pargs.append `(partial ~&arg))
                    ; (func 1 2) -> (partial func 1 2)
                    (= (type &arg) hy.models.Expression)
                    (pargs.append `(partial ~@(cut &arg 0 None)))
                    ; etc -> no change
                    True
                    (pargs.append `(partial ~&arg))))
       `(rcompose ~@pargs)
        )

; ■ comment on (.mth 3 4) deconstruction ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    ; this is how (.mth 3 4) works:
    (when False
        (defclass [dataclass] Point []
            (#^ int x)
            (#^ int y)
            (defn getXscaled [self scale1 scale2] (* scale1 scale2 self.x)))
        ;
        (   (rcompose (partial Point 7) ; -> Point(1,2)
                      (partial flip getattr "getXscaled") ; -> mth
                      (partial (fn [%args %mth] (%mth #* %args)) [3 4])
                      )
            2)
    )

; ________________________________________________________________________/ }}}2

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
                  ; .attr -> attr
                  (_isDotted &arg)
                  (setv (get args &i) (_extractDotted &arg))
                  ; (-1) -> [(- 1)]
                  (_isNegInteger &arg)
                  (setv (get args &i) [&arg])
                  ; (mth> f 1) -> (call "f" 1)
                  (_isExprWithHeadSymbol &arg "mth>")
                  (setv (get args &i) `(call ~(str (_extractDotted (get &arg 1)))
                                            ~@(get &arg (slice 2 None))))
                  ; (mut> f 1) -> (call_mut "f" 1)
                  (_isExprWithHeadSymbol &arg "mut>")
                  (setv (get args &i) `(call_mut ~(str (_extractDotted (get &arg 1)))
                                                ~@(get &arg (slice 2 None))))))
        ; process (dndr> ...) // TODO: recognise (dndr> & ...) as bitwise_and
        (setv last_arg (get args (- 1)))
        (cond (_isExprWithHeadSymbol last_arg "dndr>")
             `(->  (. lens ~@(get args (slice 0 (- 1))))
                  ~(get last_arg (slice 1 None)))
              (_isExprWithHeadSymbol last_arg "dndr>>")
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

; _____________________________________________________________________________/ }}}1
; pluckm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (defmacro pluckm [indx iterable]
        (cond ; .attr -> attr
              (_isDotted indx)
              (return `(lpluck_attr ~(str (_extractDotted indx)) ~iterable))
              ;
              True
              (return `(lpluck ~indx ~iterable))))

; _____________________________________________________________________________/ }}}1


