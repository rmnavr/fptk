
    (export :macros [ assertm           ; fptk macro
                      gives_error_typeQ ; fptk macro
                    ])

; [GROUP] Testing ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (require fptk._macros [assertm])            #_ "(assertm op arg1 arg2) | tests if (op arg1 arg2), for example (= 1 1)"
    (require fptk._macros [gives_error_typeQ])  #_ "| example: (assertm gives_error_typeQ (get [1] 2) IndexError)"

; _____________________________________________________________________________/ }}}1
