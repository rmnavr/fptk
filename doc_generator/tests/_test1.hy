
    (require hyrule [comment])

; [GROUP] Pupos riba ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import funcy) 
    (import funcy) #_ "riba"

; _____________________________________________________________________________/ }}}1
; [GROUP] Pups funcs ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ;; import_module
    (import  sys)
    (import  math)                          #_ "3rd party"

    ;; import_from_module
    (import  funcy [rpartial]) 
    (import  funcy [partial])               #_ "funcy(f, *args, ** kwargs) | description"

    ;; import_from_module_as
    (import  operator [ ne :as neq ]) 
    (import  operator [ truediv :as div ])  #_ "div(...) | description"
    (import  operator [ truediv :as div ])  #_ " | description"
    (import  operator [ truediv :as div ])  #_ " | "
    (import  operator [ truediv :as div ])  #_ "sign | "

    ;; require_macro
    (require hyrule [of])                   #_ "sign | of smth"

    ;; non_import_info: module/kind/name/signature/descr
    (comment "hyrule | macro | get   | get(xs,n) | ")
    (comment "py     | func  | slice || slice smth")
    (comment "py     | func  | slice ||")
    (comment "py     | func  | slice |sign|descr")

    ;; defined_setv
    (setv xsmth (+ 1 2)) 

    #_ "..."
    (setv ysmth (+ 1 2)) 

    ;; defined_function
    #_ "third(xs) -> elem | throws error if not found"
    (defn third [xs] (if (<= (len xs) 2) (return None) (return (get xs 2))))
    (defn rest [xs] (get xs (slice 1 None)))  

; _____________________________________________________________________________/ }}}1

