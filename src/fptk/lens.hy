
    (export :objects [ lens
                     ]
            :macros  [ lns  ; fptk macro
                       &+   ; fptk macro
                       &+>  ; fptk macro
                       l>   ; fptk macro
                       l>=  ; fptk macro
                     ])

; [GROUP] Lens ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import lenses [lens])      #_ "3rd party module (for working with immutable structures)"

    (require fptk._macros [lns])
    (require fptk._macros [&+])
    (require fptk._macros [&+>])
    (require fptk._macros [l>])
    (require fptk._macros [l>=])

; _____________________________________________________________________________/ }}}1

