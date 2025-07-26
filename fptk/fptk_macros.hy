
; Import ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import  hyrule [rest butlast])
	(require hyrule [-> ->> of])
    (import  operator)

; _____________________________________________________________________________/ }}}1

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
;
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

; === Macros ===
; f:: ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	(defmacro f:: [#* macro_args]
		;
		(setv fInputsOutputs (get macro_args (slice None None 2)))
		(setv fInputs (get fInputsOutputs (slice 0 (- 1))))
		(setv fOutput (get fInputsOutputs (- 1)))
		`(of Callable ~fInputs ~fOutput))

; _____________________________________________________________________________/ }}}1
; p: ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

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

	(defmacro p: [#* args]
		(setv pargs [])
		(for [&arg args]
			  (cond ; .x  -> (partial flip getattr "x")
					(_isDottedAttr &arg)
					(pargs.append `(hy.I.funcy.partial (fn [f x y] (f y x)) getattr ~(str (_extractDottedAttr &arg))))
					; operator.neg
					(_isDottedAccess &arg)
					(pargs.append `(hy.I.funcy.partial ~&arg))
					; (. mth 2 3) -> essentially (. SLOT mth 2 3)
					(_isDottedMth &arg)
					(do (pargs.append `(hy.I.funcy.partial (fn [f x y] (f y x)) getattr
											~(str (get (_extractDottedMth &arg) "head")))) ; -> mth)
						(pargs.append `(hy.I.funcy.partial (fn [%args %mth] (%mth (unpack_iterable  %args)))
												[~@(get (_extractDottedMth &arg) "args")])))
					; abs -> (partial abs)
					(= (type &arg) hy.models.Symbol)
					(pargs.append `(hy.I.funcy.partial ~&arg))
	                ; (fn/fm ...) -> no change
                    (or (_isExprWithHeadSymbol &arg "fn")
                        (_isExprWithHeadSymbol &arg "fm")
                        (_isExprWithHeadSymbol &arg "f>"))
                    (pargs.append &arg)
					; (func 1 2) -> (partial func 1 2)
					; (operator.add 3) -> (partial operator.add 3)
					(= (type &arg) hy.models.Expression)
					(pargs.append `(hy.I.funcy.partial ~@(cut &arg 0 None)))
					; (etc ...) -> (partial etc ...)
					True
					(pargs.append `(hy.I.funcy.partial ~&arg))))
	   `(hy.I.funcy.rcompose ~@pargs))

; _____________________________________________________________________________/ }}}1
; (l)pluckm, getattrm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	(defmacro pluckm [indx iterable]
		(cond ; .attr -> "attr"
			  (_isDottedAttr indx)
			  (return `(hy.I.funcy.pluck_attr ~(str (_extractDottedAttr indx)) ~iterable))
			  ;
			  True
			  (return `(hy.I.funcy.pluck ~indx ~iterable))))

	(defmacro lpluckm [indx iterable]
		(cond (_isDottedAttr indx) (return `(hy.I.funcy.lpluck_attr ~(str (_extractDottedAttr indx)) ~iterable))
			  True                 (return `(hy.I.funcy.lpluck ~indx ~iterable))))

	(defmacro getattrm [iterable #* args] ; first arg is «indx», second - is «default» (may be absent)
        (setv indx (get args 0))
        (cond (= (len args) 1)
              (setv default_not_given True)
              (= (len args) 2)
              (do (setv default (get args 1))
                  (setv default_not_given False)))
		(cond ; .attr -> "attr"
			  (_isDottedAttr indx)
              (if default_not_given
                  (return `(getattr ~iterable ~(str (_extractDottedAttr indx))))
                  (return `(getattr ~iterable ~(str (_extractDottedAttr indx)) ~default)))
			  ;
			  True
			  (if default_not_given
                  (return `(getattr ~iterable ~indx))
                  (return `(getattr ~iterable ~indx ~default)))))

; _____________________________________________________________________________/ }}}1
; fm, f>, (l)mapm, (l)filterm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	; recognizes "it" as solo-arg
    ; or %1..%9 as multi args
    ; 
	; "it" cannot be used together with %i
	; 
	; nested fm calls will probably not work as intended

	(defmacro fm [expr]
		(import hyrule [flatten thru])
		;
        (setv args (->> expr
                        flatten
                        (filter (fn [%x] (or (= %x 'it)
                                             (= %x '%1) (= %x '%2) (= %x '%3)
                                             (= %x '%4) (= %x '%5) (= %x '%6)
                                             (= %x '%7) (= %x '%8) (= %x '%9))))
                        sorted))    ; example: [hy.models.Symbol('%1'), hy.models.Symbol('%2')]
        (when (in 'it args) (return `(fn [it] ~expr)))
		(if (= (len args) 0)
			(setv maxN 0)
			(setv maxN (int (get args -1 -1)))) 
		(setv inputs (lfor n (thru 1 maxN) (hy.models.Symbol f"%{n}")))
		(return `(fn [~@inputs] ~expr)))

	(defmacro f> [lambda_def #* args]
		(return `((hy.R.fptk_macros.fm ~lambda_def) ~@args)))

    (defmacro mapm [one_shot_fm #* args]
		(return `(map (hy.R.fptk_macros.fm ~one_shot_fm) ~@args)))

    (defmacro lmapm [one_shot_fm #* args]
		(return `(list (map (hy.R.fptk_macros.fm ~one_shot_fm) ~@args))))

    (defmacro filterm [one_shot_fm iterable]
		(return `(filter (hy.R.fptk_macros.fm ~one_shot_fm) ~iterable)))

    (defmacro lfilterm [one_shot_fm iterable]
		(return `(list (filter (hy.R.fptk_macros.fm ~one_shot_fm) ~iterable))))

; _____________________________________________________________________________/ }}}1
; lns ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	(defmacro lns [#* macro_args]
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
; &+ &+> l> l>= ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	; compose lens, add setters/getters

	(defmacro &+ [#* macro_args]
		(setv lenses_ (butlast macro_args))
		(setv func	 (get macro_args (- 1)))
	   `(& ~@lenses_ (hy.R.fptk_macros.lns ~func)))

	; compose lens, add setters/getters, apply

	(defmacro &+> [#* macro_args]
		(setv variable (get macro_args 0))
		(setv lenses   (butlast (rest macro_args)))
		(setv func	   (get macro_args (- 1)))
	   `((& ~@lenses (hy.R.fptk_macros.lns ~func)) ~variable))

	; construct lens, apply:

	(defmacro l> [#* macro_args]
		(setv variable	  (get macro_args 0))
		(setv lenses_args (rest macro_args))
	   `((hy.R.fptk_macros.lns ~@lenses_args) ~variable))

	(defmacro l>= [#* macro_args]
		(setv variable	  (get macro_args 0))
		(setv lenses_args (rest macro_args))
	   `(&= ~variable (hy.R.fptk_macros.lns ~@lenses_args)))

; _____________________________________________________________________________/ }}}1
; assertm, errortypeQ ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	(defmacro assertm [op arg1 arg2]
        (setv to_test `(~op ~arg1 ~arg2))
        (setv _test_expr (hy.repr `(~op ~arg1 ~arg2)))
        (setv _arg1 (hy.repr arg1))
        (setv _arg2 (hy.repr arg2))
        ;
        (setv _full_expr_result True)
       `(try (assert ~to_test False)
             True ; return
             (except [eFull Exception]
                     (print "Error in" ~_test_expr "|" (type eFull) ":" eFull)
                     (setv _outp eFull)
                     (try ~arg1
                          (print ">>" ~_arg1 "=" ~arg1)
                          (except [e1 Exception]
                                  (print ">> Can't calc" ~_arg1 "|" (type e1) ":" e1)))
                     (try ~arg2
                          (print ">>" ~_arg2 "=" ~arg2)
                          (except [e2 Exception]
                                  (print ">> Can't calc" ~_arg2 "|" (type e1) ":" e2)))
                     eFull )))

	(defmacro gives_error_typeQ [expr error_type]
       `(try ~expr
             False
             (except [e Exception]
                     (= ~error_type (type e)))))

; _____________________________________________________________________________/ }}}1

