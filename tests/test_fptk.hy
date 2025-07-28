
    ; this file relies on "asertm" and "gives_error_typeQ" macros imported from tested "fptk_macros.hy" file for testing,
    ; if those macros are updated themselves, be sure that all testing is still working

; Imports (tricks to import fptk from sibling folder) ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; 1) Load fptk_funcs.hy from sibling folder
    (import sys)
    (setv _cur_path (get sys.path 0))
    (setv _cur_folder_removed (get (_cur_path.rsplit "\\tests" 1) 0))
    (setv _fptk_path (+ _cur_folder_removed "\\fptk"))
    (sys.path.append (+ _cur_folder_removed "\\fptk"))
    (import fptk_funcs *)

    ; 2) Load fptk_macros.hy from coppied file:
    (require fptk_macros *)

; _____________________________________________________________________________/ }}}1

; functions
; 0-Getters ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (first []) None)
    (assertm = (second [1]) None)
    (assertm = (third [1 2]) None)
    (assertm = (fourth [1 2 3]) None)
    (assertm = (beforelast [1]) None)
    (assertm = (last []) None)

    (assertm = (rest [1 2 3]) [2 3])
    (assertm = (rest [1]) [])
    (assertm = (rest []) [])

    (assertm = (butlast [1 2 3]) [1 2])
    (assertm = (butlast [1]) [])
    (assertm = (butlast []) [])

    (assertm = (drop 1 "2") "")
    (assertm = (drop -3 "4321") "4")
    (assertm = (drop 1 [0]) [])
    (assertm = (drop 1 [0 1 2]) [1 2])
    (assertm = (drop 10 [0 1 2]) [])
    (assertm = (drop -10 [0 1 2]) [])
    (assertm = (drop 0 [0 1 2]) [0 1 2])
    (assertm = (drop -2 [0 1 2]) [0])

    (assertm = (take 3 [1 2 3 4]) [1 2 3])
    (assertm = (take 0 []) [])
    (assertm = (take 0 [1 2 3]) [])
    (assertm = (take -1 [1]) [1])
    (assertm = (take -1 [1 2]) [2])
    (assertm = (take -100 [1 2]) [1 2])

    (assertm = (pick [0 0 0] [1 2]) [1 1 1])
    (assertm = (pick [] [1 2]) [])
    (assertm = (pick [1 2 3 -1 -2] [0 1 2 3]) [1 2 3 3 2])

; _____________________________________________________________________________/ }}}1
; 1-Getters ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (get_ [[1 2] [1 3]] 1 2) 2)
    (assertm = (get_ [[1 2] [1 3]] (- 1 2) 2) 3)
    (assertm gives_error_typeQ (get_ [1 2 3] 0) IndexError)

    (assertm = (nth_ 1  [[1 2] [1 3]]) [1 2])
    (assertm = (nth_ -1 [[1 2] [1 3]]) [1 3])
    (assertm = (nth_ -3 [[1 2] [1 3]]) None)
    (assertm = (nth_ 3 [[1 2] [1 3]]) None)
    (assertm gives_error_typeQ (nth_ 0 [1 2 3]) IndexError)

    (assertm = (get [1 2 3 4 5 6 7] (slice_ 1 3)) [1 2 3])
    (assertm = (get [1 2 3 4 5 6 7] (slice_ -3 -1)) [5 6 7])
    (assertm = (get [1 2 3 4 5 6 7] (slice_ 6 100)) [6 7])
    (assertm = (get [1 2 3 4 5 6 7] (slice_ 1 6 2)) [1 3 5])
    (assertm = (get [1 2 3 4 5 6 7] (slice_ -1000 -1)) [1 2 3 4 5 6 7])
    (assertm gives_error_typeQ (slice_ 0 3) IndexError)
    (assertm gives_error_typeQ (slice_ 3 0) IndexError)

    (assertm = (cut_ [1 2 3 4 5 6 7] 1 3) [1 2 3])
    (assertm = (cut_ [1 2 3 4 5 6 7] -3 -1) [5 6 7])
    (assertm = (cut_ [1 2 3 4 5 6 7] 6 100) [6 7])
    (assertm = (cut_ [1 2 3 4 5 6 7] 1 6 2) [1 3 5])
    (assertm = (cut_ [1 2 3 4 5 6 7] -1000 -1) [1 2 3 4 5 6 7])
    (assertm gives_error_typeQ (cut_ [1 2 3] 0 3) IndexError)
    (assertm gives_error_typeQ (cut_ [1 2 3] 3 0) IndexError)

; _____________________________________________________________________________/ }}}1
; FP ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (list (flip map [-1 -2 -3] abs))    [1 2 3])
    (assertm = (list ((pflip map [-1 -2 -3]) abs)) [1 2 3])

    (assertm = (lstarmap (fn [x y] (+ x y)) [[1 2] [3 4] [5 6]]) [3 7 11])
    (assertm = (lzip [1 2 3] [4 5 6]) [#(1 4) #(2 5) #(3 6)])

    (assertm = ((nested 3 math.sqrt) 256) (math.sqrt (math.sqrt (math.sqrt 256))))
    (assertm = (apply_n 3 math.sqrt  256) (math.sqrt (math.sqrt (math.sqrt 256))))
    (assertm = (apply_n 1 math.sqrt 256) 16)

; _____________________________________________________________________________/ }}}1
; APL ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (fltr1st evenQ [1 2 3 4 5]) 2)
    (assertm = (fltr1st evenQ []) None)
    (assertm = (list (without [1 2 3] [1 2 3 1 2 5 1 2 3 6])) [5 6])
    (assertm = (lwithout [1 2 3] [1 2 3 1 2 5 1 2 3 6]) [5 6])

    (assertm = (lbisect_at 3  [1 2 3 4 5 6 7]) (tuple [[1 2 3] [4 5 6 7]]))
    (assertm = (lbisect_at -2 [1 2 3 4 5 6 7]) (tuple [[1 2 3 4 5] [6 7]]))
    (assertm = (lbisect_at -10 [1 2 3 4 5 6 7]) (tuple [[] [1 2 3 4 5 6 7]]))
    (assertm = (lbisect_at 10 [1 2 3 4 5 6 7]) (tuple [[1 2 3 4 5 6 7] []]))
    (assertm = (lbisect_at 0 [1 2 3 4 5 6 7]) (tuple [[] [1 2 3 4 5 6 7]]))

    (assertm = (list (partition 10 [1 2 3 4])) [])
    (assertm = (list (partition 2 [1 2 3 4])) [[1 2] [3 4]])
    (assertm = (list (partition 2 :step 1 [1 2 3])) [[1 2] [2 3]])
    (assertm = (list (partition 2 :step 3 [1 2 3])) [[1 2]])
    (assertm = (list (partition 2 :step 3 [1 2 3 4 5 6])) [[1 2] [4 5]])
    (assertm = (list (partition 2 :step 3 :tail True [1 2 3 4])) [[1 2] [4]])
    (assertm = (list (partition 2 :tail True [1 2 3 4 5])) [[1 2] [3 4] [5]])

    (assertm = (lpartition 10 [1 2 3 4]) [])
    (assertm = (lpartition 2 [1 2 3 4]) [[1 2] [3 4]])
    (assertm = (lpartition 2 :step 1 [1 2 3]) [[1 2] [2 3]])
    (assertm = (lpartition 2 :step 3 [1 2 3]) [[1 2]])
    (assertm = (lpartition 2 :step 3 [1 2 3 4 5 6]) [[1 2] [4 5]])
    (assertm = (lpartition 2 :step 3 :tail True [1 2 3 4]) [[1 2] [4]])
    (assertm = (lpartition 2 :tail True [1 2 3 4 5]) [[1 2] [3 4] [5]])

; _____________________________________________________________________________/ }}}1
; lmulticut_by ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

; ■ False False ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    (assertm = (lmulticut_by :keep_border False :merge_border False (fn [it] (eq True it)) [])
               [])

    (assertm = (lmulticut_by :keep_border False :merge_border False (fn [it] (eq True it)) [True True False False True True True False True True True False True])
               [[] [False False] [] [] [False] [] [] [False] []])

    (assertm = (lmulticut_by :keep_border False :merge_border False (fn [it] (eq True it)) [True True])
               [[] []])

    (assertm = (lmulticut_by :keep_border False :merge_border False (fn [it] (eq True it)) [True])
               [[]])

    (assertm = (lmulticut_by :keep_border False :merge_border False (fn [it] (eq True it)) [False False])
               [[False False]])

    (assertm = (lmulticut_by :keep_border False :merge_border False (fn [it] (eq True it)) [False])
               [[False]])

    (assertm = (lmulticut_by :keep_border False :merge_border False (fn [it] (eq True it)) [True False True])
               [[False] []])

    (assertm = (lmulticut_by :keep_border False :merge_border False (fn [it] (eq True it)) [True False False True True False])
               [[False False] [] [False]])

; ________________________________________________________________________/ }}}2
; ■ False True ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    (assertm = (lmulticut_by :keep_border False :merge_border True  (fn [it] (eq True it)) [])
               [])

    (assertm = (lmulticut_by :keep_border False :merge_border True  (fn [it] (eq True it)) [True True False False True True True False True True True False True])
               [[False False] [False] [False]])

    (assertm = (lmulticut_by :keep_border False :merge_border True  (fn [it] (eq True it)) [True True])
               [])

    (assertm = (lmulticut_by :keep_border False :merge_border True  (fn [it] (eq True it)) [True])
               [])

    (assertm = (lmulticut_by :keep_border False :merge_border True  (fn [it] (eq True it)) [False False])
               [[False False]])

    (assertm = (lmulticut_by :keep_border False :merge_border True  (fn [it] (eq True it)) [False])
               [[False]])

    (assertm = (lmulticut_by :keep_border False :merge_border True  (fn [it] (eq True it)) [True False True])
               [[False]])

    (assertm = (lmulticut_by :keep_border False :merge_border True  (fn [it] (eq True it)) [True False False True True False])
               [[False False] [False]])

; ________________________________________________________________________/ }}}2
; ■ True  False ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    (assertm = (lmulticut_by :keep_border True  :merge_border False (fn [it] (eq True it)) [])
               [])

    (assertm = (lmulticut_by :keep_border True  :merge_border False (fn [it] (eq True it)) [True True False False True True True False True True True False True])
               [[True] [True False False] [True] [True] [True False] [True] [True] [True False] [True]])

    (assertm = (lmulticut_by :keep_border True  :merge_border False (fn [it] (eq True it)) [True True])
               [[True] [True]])

    (assertm = (lmulticut_by :keep_border True  :merge_border False (fn [it] (eq True it)) [True])
               [[True]])

    (assertm = (lmulticut_by :keep_border True  :merge_border False (fn [it] (eq True it)) [False False])
               [[False False]])

    (assertm = (lmulticut_by :keep_border True  :merge_border False (fn [it] (eq True it)) [False])
               [[False]])

    (assertm = (lmulticut_by :keep_border True  :merge_border False (fn [it] (eq True it)) [True False True])
               [[True False] [True]])

    (assertm = (lmulticut_by :keep_border True  :merge_border False (fn [it] (eq True it)) [True False False True True False])
               [[True False False] [True] [True False]])

; ________________________________________________________________________/ }}}2
; ■ True  True ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    (assertm = (lmulticut_by :keep_border True  :merge_border True  (fn [it] (eq True it)) [])
               [])

    (assertm = (lmulticut_by :keep_border True  :merge_border True  (fn [it] (eq True it)) [True True False False True True True False True True True False True])
               [[True True False False] [True True True False] [True True True False] [True]])

    (assertm = (lmulticut_by :keep_border True  :merge_border True  (fn [it] (eq True it)) [True True])
               [[True True]])

    (assertm = (lmulticut_by :keep_border True  :merge_border True  (fn [it] (eq True it)) [True])
               [[True]])

    (assertm = (lmulticut_by :keep_border True  :merge_border True  (fn [it] (eq True it)) [False False])
               [[False False]])

    (assertm = (lmulticut_by :keep_border True  :merge_border True  (fn [it] (eq True it)) [False])
               [[False]])

    (assertm = (lmulticut_by :keep_border True  :merge_border True  (fn [it] (eq True it)) [True False True])
               [[True False] [True]])

    (assertm = (lmulticut_by :keep_border True  :merge_border True  (fn [it] (eq True it)) [True False False True True False])
               [[True False False] [True True False]])

; ________________________________________________________________________/ }}}2

; _____________________________________________________________________________/ }}}1
; Math ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (normalize [3 0 0]) [1 0 0])

    (setv _norm (py "math.sqrt(1+4+4)"))
    (assertm = (normalize [1 2 2]) (list (map (fn [x] (operator.truediv x _norm)) [1 2 2])))

    ; basic python */+ act as a monoid on numbers, but can't work on 1-arg "" or []:
    (assertm = (+ 3) 3)
    (assertm = (+)   0)
    (assertm = (* 3) 3)
    (assertm = (*)   1)
    (assertm gives_error_typeQ (+ "pups") TypeError)
    (assertm gives_error_typeQ (+ [1]) TypeError)

    ; same as */+ :
    (assertm = (dadd 3) 3)
    (assertm = (dadd)   0)
    (assertm = (dmul 3) 3)
    (assertm = (dmul)   1)
    (assertm gives_error_typeQ (dadd "pups") TypeError)
    (assertm gives_error_typeQ (dadd [1]) TypeError)

    (assertm = (smul 3 "a") "aaa")
    (assertm = (lmul 3 [1]) [1 1 1])

    ; monoids:
    ; monoids:
    (assertm = (mul) 1)
    (assertm = (mul 3) 3)
    (assertm = (mul 7 0) 0)
    (assertm = (mul 2 2 3) 12)

    (assertm = (plus) 0)
    (assertm = (plus 3) 3)
    (assertm = (plus 7 0) 7)
    (assertm = (plus 2 2 3) 7)

    (assertm = (lconcat) [])
    (assertm = (lconcat [1] [] [2]) [1 2])

    (assertm = (sconcat) "")
    (assertm = (sconcat "a" "b" "") "ab")


; _____________________________________________________________________________/ }}}1
; Checks ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1
    
    (assertm = (zerolenQ "") True)
    (assertm = (zerolenQ []) True)
    (assertm = (zerolenQ "a") False)
    (assertm = (zerolenQ [None]) False)

    (assertm = (oflenQ "123" 3) True)
    (assertm = (oflenQ "123" -3) False)
    (assertm = (oflenQ "" 0) True)
    (assertm = (oflenQ [1 2 3] 3) True)
    (assertm = (oflenQ [1 2 3] -3) False)
    (assertm = (oflenQ [] 0) True)

    (assertm = (oftypeQ str "1") True)
    (assertm = (oftypeQ int "1") False)

    (assertm = (on len eq "123" ["a" 2 "c"]) True)
    (assertm = (on len eq "" []) True)
    (assertm = (on len eq "" "a") False)

    (assertm = (listQ []) True)
    (assertm = (listQ list) False)
    (assertm = (listQ (list)) True)

    (assertm = (fnot eq 1 2) True)
    (assertm = (fnot eq 1 1) False)

; _____________________________________________________________________________/ }}}1
; Strings ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (str_join ["a" "b" "" "c"]) "abc")
    (assertm = (str_join :sep "-" ["a" "b" "" "c"]) "a-b--c")

    (assertm = (str_replace "pupsububr" "u" "R") "pRpsRbRbr")
    (assertm = (str_replace :count 1 "pupsububr" "u" "R") "pRpsububr")

    (assertm = (strip " bubr ") "bubr")
    (assertm = (strip :chars "|" "Ybubr|") "Ybubr")
    (assertm = (strip :chars "|Ybr" "Ybubr|") "u")
    (assertm = (lstrip " bubr ") "bubr ")
    (assertm = (lstrip :chars "|" "Ybubr|") "Ybubr|")
    (assertm = (lstrip :chars "|Ybr" "Ybubr|") "ubr|")
    (assertm = (rstrip " bubr ") " bubr")
    (assertm = (rstrip :chars "|" "Ybubr|") "Ybubr")
    (assertm = (rstrip :chars "|Ybr" "Ybubr|") "Ybu")

    (assertm = (enlengthen 3 " ") (* " " 3))
    (assertm = (enlengthen 0 "pups") "pups")
    (assertm = (enlengthen -10 "pups") "pups")
    (assertm = (enlengthen 6 "pups" :char "-") "pups--")
    (assertm = (enlengthen 6 "pups" :char "-" :fill_tail False) "--pups")
    (assertm = (enlengthen 4 "pups" :char "-" :fill_tail False) "pups")

; _____________________________________________________________________________/ }}}1

; macros
; f:: ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (f:: int -> int => int) (of Callable [int int] int))
    (assertm = (f:: int -> int -> (of Tuple int str)) (of Callable [int int] (of Tuple int str)))

; _____________________________________________________________________________/ }}}1
; p: ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (lmap (p: (fn [x] x)           ; fn
                         (fm it)              ; fm macro
                         (abs)                ; function
                         (operator.add 4)     ; function
                         str                  ; function
                         (.__contains__ "2")  ; method access
                         .__class__)          ; attribute access
                      [1 2 3])
               (lmap type [True True True]))

; _____________________________________________________________________________/ }}}1
; (l)pluckm, getattrm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (type (pluckm 0 [[1 2 3] [4 5 6]])) (type (map abs [1 4])))
    (assertm = (list (pluckm 0 [[1 2 3] [4 5 6]])) [1 4])
    (assertm = (lpluckm 0 [[1 2 3] [4 5 6]]) [1 4])
    (assertm = (lpluckm 0 [[1 2 3] [4 5 6]]) [1 4])

    (defclass [dataclass] Point [] (#^ int x) (#^ int y))

    (assertm = (list (pluckm .x [(Point 1 2) (Point 3 4)])) [1 3])
    (assertm = (lpluckm .x [(Point 1 2) (Point 3 4)]) [1 3])

    (assertm = (getattrm (Point 1 2) .x) 1)
    (assertm = (getattrm (Point 1 2) "y") 2)

; _____________________________________________________________________________/ }}}1
; fm, f>, (l)mapm, (l)filterm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = ((fm (+ %1 %3 1)) 1 2 3) 5)
    (assertm = ((fm (+ it 1)) 3) 4)
    (assertm gives_error_typeQ ((fm (+ it %1 1)) 3) NameError)
    (assertm gives_error_typeQ ((fm (+ it %1 1)) 3 4) TypeError)

    (assertm = (f> (+ %1 %3 1) 1 2 3) 5)
    (assertm = (f> (+ it 1) 3) 4)
    (assertm gives_error_typeQ (f> (+ it %1 1) 3) NameError)
    (assertm gives_error_typeQ (f> (+ it %1 1) 3 4) TypeError)

    (assertm = (list (mapm (+ it 1) [1 2 3])) [2 3 4])
    (assertm = (lmapm (+ %1 1) [1 2 3]) [2 3 4])
    (assertm = (lfilterm (= it True) [1 0 1]) [1 1])
    (assertm = (list (filterm (= it True) [1 0 1])) [1 1])

; _____________________________________________________________________________/ }}}1
; lns ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv idx 3)

    (assertm = (  lns   1   -2 (- 7)  "key"   idx)
               (. lens [1] [-2] [-7] ["key"] [idx]))

    (assertm = (  lns   .attr)
               (. lens   attr))

    (assertm = ((  lns  (Each) (set 3)) [1 2 3])
               ((. lens (Each) (set 3)) [1 2 3]))

    (assertm gives_error_typeQ (lns (+ idx 3)) TypeError)

    (assertm = (  lns  [(+ idx 3)])
               (. lens [(+ idx 3)]))

    (assertm = ((  lns   1  (mth> .sort))  [[1 2] [3 4]])
               ((. lens [1] (call "sort")) [[1 2] [3 4]]))

    (assertm = ((  lns   1  (mut>     .copy  :shallow True)) [[1 2] [3 4]])
               ((. lens [1] (call_mut "copy" :shallow True)) [[1 2] [3 4]]))

    (assertm = ((lns 1 (dndr> / 3)) [1 2 3])
               ((/ (. lens [1]) 3)  [1 2 3]))

    (assertm = ((lns 1 (dndr>> / 3)) [1 2 3])
               ((/ 3 (. lens [1]))   [1 2 3]))

; _____________________________________________________________________________/ }}}1
; &+ &+> l> l>= ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (l> [[1 2 3] [4 5 6]]          1  (Each) (modify sqrt))
               (&  [[1 2 3] [4 5 6]] (. lens [1] (Each) (modify sqrt))))   ; btw & can accept multiple lens

    (assertm = (do (setv xs [[1 2 3] [4 5 6]])
                   (l>= xs          1  (Each) (modify sqrt))
                   xs)
               (do (setv xs [[1 2 3] [4 5 6]])
                   (&=  xs (. lens [1] (Each) (modify sqrt)))
                   xs))

    (assertm = ((&+ (  lns   1)  (. lens [2]) (set "here")) [[1 2 3] [4 5 6]])
               ((&  (. lens [1]) (. lens [2] (set "here"))) [[1 2 3] [4 5 6]]))   ; btw & can accept multiple lens

    (assertm = (&+> [[1 2 3] [4 5 6]] (lns 1) (mut> .sort))
               ((& (. lens [1] (call_mut "sort"))) [[1 2 3] [4 5 6]]))   ; btw & can accept multiple lens

; _____________________________________________________________________________/ }}}1

; ===========================================================
    (print "== TESTING COMPLETED (if there are no errors above, all is good) ==")
