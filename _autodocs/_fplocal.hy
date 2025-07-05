
; Import Full Modules ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import sys)
    (sys.stdout.reconfigure :encoding "utf-8")

    (import math
            operator
            random
            re
            itertools
            functools
            pprint [pprint]
            ; 
            hyrule
            funcy)

; _____________________________________________________________________________/ }}}1
; Typing ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (require hyrule [of])

    (import ; Basic:
            dataclasses     [dataclass]
            enum            [Enum]
            abc             [ABC abstractmethod]
            typing          [List Tuple TypedDict Dict Union Generator Any Optional Callable Literal Type]
            ; 3rd party:
            pydantic        [BaseModel StrictInt StrictStr StrictFloat validate_arguments]
            returns.result  [Result Success Failure])

    (import funcy [ isnone notnone ]) 

    (setv StrictNumber (get Union #(StrictInt StrictFloat)))

; _____________________________________________________________________________/ }}}1

; Getters ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import  lenses [ lens ])

    ; ------------------------------------------

    (import  funcy  [ first second last nth ; nth works with dicts
                      lpluck lpluck_attr ]  ; lpluck works with dicts
             hyrule [ assoc ])
    (require hyrule [ ncut ])

    (defn third      [xs] (if (<= (len xs) 2) (return None) (return (get xs 2))))
    (defn fourth     [xs] (if (<= (len xs) 3) (return None) (return (get xs 3))))
    (defn beforelast [xs] (if (<= (len xs) 1) (return None) (return (get xs -2))))

    (defn rest       [xs] (get xs (slice 1 None)))  ; [_ 2 3 4 5]  
    (defn butlast    [xs] (get xs (slice None -1))) ; [1 2 3 4 _]  
    (defn drop       [n xs] (if (>= n 0) (cut xs n None) (cut xs None n)))              ; drops from start/end of the list
    (defn take       [n xs] (if (>= n 0) (cut xs None n) (cut xs (+ (len xs) n) None))) ; takes from start/end of the list
    (defn pick       [ns xs] (lfor &n ns (get xs &n))) ; also works with dicts keys

; _____________________________________________________________________________/ }}}1
; APL ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; control flow:

        (require hyrule [case branch unless lif])

    ; mapping:

        (import functools [ reduce ]  ; THEORY: reduce + monoid = 2-arg-function for free becomes n-arg-function
                itertools [ starmap ]
                funcy     [ lmap ])

        (defn lstarmap [f xs] (list (starmap f xs)))

    ; filtering:

        (import funcy     [ lfilter ])

        (defn fltr1st [f xs] (next (gfor &x xs :if (f &x) &x) None))
        (defn count_occurrences [elem container] (container.count elem))

    ; list manipulate:

        (import itertools  [ cycle ])
        (import hyrule     [ thru flatten ])

        (defn lprint [lst] (lmap print lst) (return None))
        (defn lzip [#* args] (list (zip #* args)))
        (defn lreversed [xs] (list (reversed xs)))

; _____________________________________________________________________________/ }}}1
; Compositions ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; apply:

        (require hyrule [ do_n list_n ])

        (defn apply_n [f n arg] ((compose #* (list_n n f)) arg))

        (import funcy [ ljuxt ])

    ; utils:

        (import hyrule [ constantly ])
        (import funcy  [ identity ])

    ; composition:

        (require hyrule [ as-> -> ->> doto ])

        (import funcy   [ curry autocurry partial rpartial compose rcompose ])

        (defn flip [f a b] (f b a))                   ; (flip lmap [1 2 3] sqrt)
        (defn pflip [f a] (fn [%x] (f %x a)))         ; (lmap (pflip div 0.1) (thru 1 3))

; _____________________________________________________________________________/ }}}1
; Math, +/* synonims ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; sign:

        (import hyrule    [ inc dec sign ])
        (import operator  [ neg ])

        (defn half       [x] (/ x 2))
        (defn double     [x] (* x 2))
        (defn reciprocal [x] (/ 1 x))

    ; dist:

        (import math [ sqrt dist hypot ])

        (defn normalize [v0]
            (setv norm (hypot #* v0))
            (if (!= norm 0) (return (lmap (pflip div norm) v0)) (return v0)))

    ; product:

        (import operator [ truediv :as div ])
        (import math     [ prod :as product log exp log10])
        (defn ln [x] (log x)) ; so, always with base = math.e
 
    ; trig:

        (import math [ pi sin cos tan degrees radians acos asin atan atan2 ])

    ; */+

        (defn minus   [x y] (- x y))

        (defn mul     [#* args] (* #* args))    ; just a synonim for * (but a function, not macros)
        (defn smul    [#* args] (* #* args))    ; underlines usage for strings
        (defn lmul    [#* args] (* #* args))    ; underlines usage for lists

        (defn plus    [#* args] (+ #* args))    ; just a synonim for + (but a function, not macros)
        (defn sconcat [#* args] (+ #* args))    ; underlines usage for strings
        (defn lconcat [#* args] (if (= (len args) 1) (first args) (+ #* args)))    ; underlines usage for lists

; _____________________________________________________________________________/ }}}1

; Logic, Checks (Q) ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import hyrule    [ xor ]
            operator  [ eq ne :as neq ]
            funcy     [ even odd ])

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

; ■ theory ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    ; re_find(regex_patters, string, flags=0), re_test -> match, re_all -> list of all matches
    ; re_sub(regex_pattern, replacement, string, count=0, flags=0)
    ; re_split(r"\s+", text)

    ; re: match, search, findall, finditer, split, compile, fullmatch, escape

    ; non-escaped (commands):   .  ^  $  *  +  ? {2,4} [abc]      ( | )
    ; escaped (literals):      \. \^ \$ \* \+ \? \{ \} $$_bracket $_parenthesis \| \\ \' \"
    ; special:                 \d \D \w \W \s \S \b \B \n \r \f \v
    ; raw strings:             r"\d+" = "\\d+"

; ________________________________________________________________________/ }}}2

    (import re        [ sub :as re_sub split :as re_split ])
    (import funcy     [ re_find re_test re_all ])

; _____________________________________________________________________________/ }}}1
; Random ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import random    [ choice
                        randint
                        uniform :as randfloat
                        random :as rand01 ])

                        ; shuffle — is mutating

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

	; === Helpers ===

; neg integer expr ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	; (- 1)
	(-> (defn _isNegIntegerExpr
			[ arg
			]
			(and (= (type arg) hy.models.Expression)
				 (= (get arg 0) (hy.models.Symbol "-"))
				 (= (len arg) 2)
				 (= (type (get arg 1)) hy.models.Integer)))
		eval_and_compile)

	; (_isNegIntegerExpr '(- 3))

; _____________________________________________________________________________/ }}}1
; expr with head symbol ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	; (head ...)
	(-> (defn _isExprWithHeadSymbol
			[ #^ hy.models.Expression arg
			  #^ str head
			]
			(and (= (type arg) hy.models.Expression)
				 (= (get arg 0) (hy.models.Symbol head))))
		eval_and_compile)

; _____________________________________________________________________________/ }}}1

; DEVDOC: Dot Macro Expressions ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1
(when False

	;in normal code:
		'obj.attr				; / access attr (no call)  ; [. obj attr]
		'(. obj attr)			; \
		'(. obj (mth arg1))		; access mth (and call)    ; [. obj [mth arg1]]
		'(.mth obj arg1)		; access mth (and call)    ; [[. None mth] obj arg1]
		'(obj.mth 3)			; access mth (and call)    ; [[. obj mth] 3]

	;in hyrule -> macro:
		'(-> obj func1 .attr)			; will give error, since can't call
		'(-> obj func1 (. obj2 mth 1))	; will give error, since can't parse to correct place
		;
		'(-> obj obj2.mth)				; will expand to: (obj2.mth obj)
		'(-> obj (obj2.mth 2))			; will expand to: (obj2.mth obj 2)
		'(-> obj func1 .method)			; will expand to: (. (func1 obj) (method))
		'(-> obj func1 (.method 1))		; will expand to: (. (func1 obj) (method 1))
		'(-> obj func1 (. obj2 mth))	; will expand to: (obj2.mth (func1 obj))

	; used in my p> macro:
		'.attr					; dottedAttr	; [. None attr]			; ~ get attr
		'operator.neg			; dottedAccess	; [. operator neg]		; (partial operator.neg)
		'(operator.add 3)		; dottedCall	; [[. operator add] 3]	; (partial operator.add 3)
		'(.mth)					; dottedMth		; [[. None mth]]		; ~(partial obj.mth 1 2)
		'(.mth 1 2)				; dottedMth		; [[. None mth] 1 2]	; ~(partial obj.mth 1 2)

)
; _____________________________________________________________________________/ }}}1
; .dottedAttr ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	; .x
	(-> (defn _isDottedAttr
			[ arg
			]
			(and (= (type arg) hy.models.Expression)
				 (= (get arg 0) (hy.models.Symbol "."))
				 (= (get arg 1) (hy.models.Symbol "None"))))
		eval_and_compile)

	; hy.models.Symbol[x]
	(-> (defn #^ hy.models.Symbol
			_extractDottedAttr
			[ arg
			]
			(get arg 2))
		eval_and_compile)

; _____________________________________________________________________________/ }}}1
; .dottedAccess ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	; operator.add
	(-> (defn _isDottedAccess
			[ arg
			]
			(and (= (type arg) hy.models.Expression)
				 (= (get arg 0) (hy.models.Symbol "."))
				 (= (type (get arg 1)) hy.models.Symbol)
				 (!= (get arg 1) (hy.models.Symbol "None"))))
		eval_and_compile)

; _____________________________________________________________________________/ }}}1
; .dottedMth ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	; (.mth obj 1 2)
	;  ---- -------
	;  head  args
	(-> (defn _isDottedMth
			[ arg
			]
			(and (= (type arg) hy.models.Expression)
				 (_isDottedAttr (get arg 0))))
		eval_and_compile)

	(-> (defn _extractDottedMth
			[ arg
			]
			(dict :head (get arg 0 2)
				  :args (cut arg 1 None)))
		eval_and_compile)

	 ; (_isDottedMth '(.obj obj 1 2))
	 ; (_extractDottedCall '(.obj obj 1 2))

; _____________________________________________________________________________/ }}}1
; [ARCHIVE] :attr: ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

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

; _____________________________________________________________________________/ }}}1

	; === Macroses ===

; f:: ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	(defmacro f:: [#* macro_args]
		;
		(setv fInputsOutputs (get macro_args (slice None None 2)))
		(setv fInputs (get fInputsOutputs (slice 0 (- 1))))
		(setv fOutput (get fInputsOutputs (- 1)))
		`(of Callable ~fInputs ~fOutput))

; _____________________________________________________________________________/ }}}1
; p> ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

; ■ comment on (.mth 3 4) deconstruction ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

	; this is how (.mth 3 4) works:

	;	(defclass [dataclass] Point []
	;		(#^ int x)
	;		(#^ int y)
	;		(defn getXscaled [self scale1 scale2] (* scale1 scale2 self.x)))
	;	
	;	; will be assembled to: Point(7,2).getXscaled(3,4)
	;	(	(rcompose (partial Point 7)
	;				  (partial flip getattr "getXscaled")					; \
	;				  (partial (fn [%args %mth] (%mth #* %args)) [3 4]))	; /
	;		2)

; ________________________________________________________________________/ }}}2

	; .attr				; [. None attr]			; _isDottedAttr   checks for: [. None _]
	; (.mth 1 2)		; [[. None mth] 1 2]	; _isDottedMth	  checks for: [[. None _] _]
	; operator.neg		; [. operator neg]		; _idDottetAccess checks for: [. smth _]
	; (operator.add 3)	; [[. operator add] 3]

    (defn flip [f a b] (f b a))                   ; (flip lmap [1 2 3] sqrt)

	(defmacro p> [#* args]
		;
		(setv pargs [])
		(for [&arg args]
			  (cond ; .x  -> (partial flip getattr "x")
					(_isDottedAttr &arg)
					(pargs.append `(partial flip getattr ~(str (_extractDottedAttr &arg))))
					; operator.neg
					(_isDottedAccess &arg)
					(pargs.append `(partial ~&arg))
					; (. mth 2 3) -> essentially (. obj mth 2 3)
					(_isDottedMth &arg)
					(do (pargs.append `(partial flip getattr
											~(str (get (_extractDottedMth &arg) "head")))) ; -> mth)
						(pargs.append `(partial (fn [%args %mth] (%mth (unpack_iterable  %args)))
												[~@(get (_extractDottedMth &arg) "args")])))
					; abs -> (partial abs)
					(= (type &arg) hy.models.Symbol)
					(pargs.append `(partial ~&arg))
					; (func 1 2) -> (partial func 1 2)
					; (operator.add 3) -> (partial operator.add 3)
					(= (type &arg) hy.models.Expression)
					(pargs.append `(partial ~@(cut &arg 0 None)))
					; etc -> no change
					True
					(pargs.append `(partial ~&arg))))
	   `(rcompose ~@pargs))

; _____________________________________________________________________________/ }}}1
; pluckm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	(defmacro pluckm [indx iterable]
		(cond ; .attr -> attr
			  (_isDottedAttr indx)
			  (return `(lpluck_attr ~(str (_extractDottedAttr indx)) ~iterable))
			  ;
			  True
			  (return `(lpluck ~indx ~iterable))))

; _____________________________________________________________________________/ }}}1
; #L (rename of hyrule #% macro) ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	(defreader L
	  (import hyrule [flatten inc])
	  ;
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
; fm, f> ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	; recognizes %1..%9 as arguments
	; nested fm calls will not work as intended

	(defmacro fm [expr]
		(import hyrule [flatten thru])
		;
		(setv models (flatten expr))
		(setv args (filter (fn [%x] (= (type %x) hy.models.Symbol)) models))	; Symbols
		(setv args (filter (fn [%x] (and (= (get %x 0) "%")						; "%_"
										 (= (len %x) 2)))
									args))
		(setv args (filter (fn [%x] (and (.isdigit (get %x 1))					; "%1..%9"
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

	(defmacro f> [lambda_def #* args]
		(return `((fm ~lambda_def) ~@args)))

; _____________________________________________________________________________/ }}}1
; lns ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	(defmacro lns [#* macro_args]
		(import hyrule [rest])
		;
		(setv args (list macro_args)) ; for mutations
		(for [[&i &arg] (enumerate args)]
			(cond ; Integer/String/Symbol
				  (or (= (type &arg) hy.models.Integer)  ; 1 -> [1]
					  (= (type &arg) hy.models.String)	 ; "str" -> ["str"]
					  (= (type &arg) hy.models.Symbol))  ; vrbl -> [vrbl]
				  (setv (get args &i) [&arg])
				  ; .attr -> attr
				  (_isDottedAttr &arg)
				  (setv (get args &i) (_extractDottedAttr &arg))
				  ; (-1) -> [(- 1)]
				  (_isNegIntegerExpr &arg)
				  (setv (get args &i) [&arg])
				  ; (mth> f 1) -> (call "f" 1)
				  (_isExprWithHeadSymbol &arg "mth>")
				  (setv (get args &i) `(call ~(str (_extractDottedAttr (get &arg 1)))
											~@(get &arg (slice 2 None))))
				  ; (mut> f 1) -> (call_mut "f" 1)
				  (_isExprWithHeadSymbol &arg "mut>")
				  (setv (get args &i) `(call_mut ~(str (_extractDottedAttr (get &arg 1)))
												~@(get &arg (slice 2 None))))))
		; process (dndr> ...):
		(setv last_arg (get args (- 1)))
		(cond (_isExprWithHeadSymbol last_arg "dndr>")
			 `(->  (. lens ~@(get args (slice 0 (- 1))))
				  ~(get last_arg (slice 1 None)))
			  (_isExprWithHeadSymbol last_arg "dndr>>")
			 `(->> (. lens ~@(get args (slice 0 (- 1))))
				  ~(get last_arg (slice 1 None)))
			  True
			 `(. lens ~@args)))

; _____________________________________________________________________________/ }}}1
; &+, &+>, l>, l>= ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	; compose lens, add setters/getters

	(defmacro &+ [#* macro_args]
		(import hyrule [rest butlast])
		;
		(setv lenses (butlast macro_args))
		(setv func	 (get macro_args (- 1)))
	   `(& ~@lenses (lns ~func)))

	; compose lens, add setters/getters, apply

	(defmacro &+> [#* macro_args]
		(import hyrule [rest butlast])
		;
		(setv variable (get macro_args 0))
		(setv lenses   (butlast (rest macro_args)))
		(setv func	   (get macro_args (- 1)))
	   `((& ~@lenses (lns ~func)) ~variable))

	; construct lens, apply:

	(defmacro l> [#* macro_args]
		(import hyrule [rest])
		;
		(setv variable	  (get macro_args 0))
		(setv lenses_args (rest macro_args))
	   `((lns ~@lenses_args) ~variable))

	(defmacro l>= [#* macro_args]
		(import  hyrule [rest])
		;
		(setv variable	  (get macro_args 0))
		(setv lenses_args (rest macro_args))
	   `(&= ~variable (lns ~@lenses_args)))

; _____________________________________________________________________________/ }}}1

