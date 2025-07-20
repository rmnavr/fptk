
	(require hyrule [->])

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
