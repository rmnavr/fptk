
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

    (require fptk._macros [lns])   #_ "macros for working with lens, see lens macros docs for details"
    (require fptk._macros [&+])    #_ "macros for working with lens, see lens macros docs for details"
    (require fptk._macros [&+>])   #_ "macros for working with lens, see lens macros docs for details"
    (require fptk._macros [l>])    #_ "macros for working with lens, see lens macros docs for details"
    (require fptk._macros [l>=])   #_ "macros for working with lens, see lens macros docs for details"

; _____________________________________________________________________________/ }}}1

