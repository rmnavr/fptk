
	(require hyrule [->])

; Macro <<- ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	(-> (defn _dotted_from_hyrules_argmove [node]
			"turns '.name forms into '(.name) forms"
			(if (and (isinstance node hy.models.Expression)
					 (= (get node 0) '.))
			   `(~node)
				node))
		eval_and_compile)

	(defmacro <<- [#* args_input]
		(setv [head args] (let [[head #* tail] (reversed args_input)] [head tail])) 
		(setv outp head)
		(for [node args :setv node (_dotted_from_hyrules_argmove node)]
			 (setv outp (if (isinstance node hy.models.Expression)
						   `(~@node ~outp)
						   `(~node ~outp))))
		outp)

; _____________________________________________________________________________/ }}}1
; Macro <- ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

	(-> (defn _rest_from_hyrules_iterables [coll]
			(import itertools)	
			(itertools.islice coll 1 None))
		eval_and_compile)

	(defmacro <- [#* args_input]
		(setv [head args] (let [[head #* tail] (reversed args_input)] [head tail])) 
		(setv outp head)
		(for [node args :setv node (_dotted_from_hyrules_argmove node)]
			 (setv outp (if (isinstance node hy.models.Expression)
						   `(~(get node 0) ~outp ~@(_rest_from_hyrules_iterables node))
						   `(~node ~outp))))
		outp)

; _____________________________________________________________________________/ }}}1

	; tests:
	(<<- print (** 3) 2)	
	(<- print (** 3) 2)	

