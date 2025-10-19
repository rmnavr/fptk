
    (import  fptk *)
    (require fptk *)

    (import math)
    (import operator)

; apl.hy ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (lfilter evenQ [1 2 3 4]) [2 4])
    (assertm = (fltr1st evenQ [1 2 3 4 5]) 2)
    (assertm = (fltr1st evenQ []) None)

    (assertm = (lfilter_split evenQ [1 2 3 4]) #([2 4] [1 3]))

    (assertm = (lmask_sel [1 2 3] [1 0 1]) [1 3])
    (assertm = (lmask_sel ["pups" "riba" "gus" "kroks"] [0 1 0 1 0 0 1 0]) ["riba" "kroks"])
    (assertm = (lmask_sel ["pups" "riba" "gus" "kroks"] (idxs2mask [0 3])) ["pups" "kroks"])

    (assertm = (mask2idxs [0 1 0 1]) [1 3])
    (assertm = (mask2idxs [1 0 1 0 0]) [0 2])
    (assertm = (mask2idxs []) [])
    (assertm = (mask2idxs [0 0]) [])
    (assertm = (mask2idxs [True False True 0 1]) [0 2 4])

    (assertm = (idxs2mask [2 4]) [0 0 1 0 1])
    (assertm = (idxs2mask [2 4] :bools True) [False False True False True])
    (assertm = (idxs2mask [] :bools True) [])
    (assertm = (idxs2mask [0 3]) [1 0 0 1])
    (assertm = (idxs2mask []) [])
    (assertm = (idxs2mask [1] :bools True) [False True])

    (assertm = (list (without [1 2 3] [1 2 3 1 2 5 1 2 3 6])) [5 6])
    (assertm = (lwithout [1 2 3] [1 2 3 1 2 5 1 2 3 6]) [5 6])

    (assertm = (list (takewhile (partial gt 3) [1 2 3 4])) [1 2])
    (assertm = (list (dropwhile (partial gt 3) [1 2 3 4])) [3 4])

    (assertm = (lbisect_at 3  [1 2 3 4 5 6 7]) (tuple [[1 2 3] [4 5 6 7]]))
    (assertm = (lbisect_at -2 [1 2 3 4 5 6 7]) (tuple [[1 2 3 4 5] [6 7]]))
    (assertm = (lbisect_at -10 [1 2 3 4 5 6 7]) (tuple [[] [1 2 3 4 5 6 7]]))
    (assertm = (lbisect_at 10 [1 2 3 4 5 6 7]) (tuple [[1 2 3 4 5 6 7] []]))
    (assertm = (lbisect_at 0 [1 2 3 4 5 6 7]) (tuple [[] [1 2 3 4 5 6 7]]))
    (assertm = (lbisect_by (partial gt 3) [1 2 3 4 5 6 7]) #([1 2] [3 4 5 6 7]))

    (assertm = (lislice (inf_range 10) 3) [10 11 12])
    (assertm = (lislice (cycle "ab") 3) ["a" "b" "a"])
    (assertm = (lcycle "abc" 4) ["a" "b" "c" "a"])
    (assertm = (lrepeat 1 3) [1 1 1])
    (assertm = (lcycle [1 2 3] 4) [1 2 3 1])
    (assertm = (lcycle [1 2] 5) [1 2 1 2 1])
    (assertm = (lcycle [] 5) [])
    (assertm = (lrepeat 10 3) [10 10 10])
    (assertm = (lrepeat "pups" 2) ["pups" "pups"])
    (assertm = (lrepeat "" 2) ["" ""])
    (assertm = (lrepeat [] 2) [[] []])

    (assertm = (lconcat [1 2 3] ["bubr"]) [1 2 3 "bubr"])
    (assertm = (lconcat [] []) [])

    (assertm = (lcat [[1 2] [3 4]]) [1 2 3 4])

    (assertm = (lmap double [1 2 3]) [2 4 6])
    ; lmapcat
    (assertm = (list (pairwise [1 2 3])) [#(1 2) #(2 3)])
    ; with_prev
    ; with_next

    (assertm = (lreversed [1 2 3]) [3 2 1])

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

    (assertm = (lpartition_by evenQ [1 2 2 3 4 4 5 5 6 7]) [[1] [2 2] [3] [4 4] [5 5] [6] [7]])

    ; group_by

; ■ lmulticut_by ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

; ■ ■ False False ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{3

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

; ___________________________________________________________________/ }}}3
; ■ ■ False True ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{3

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

; ___________________________________________________________________/ }}}3
; ■ ■ True  False ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{3

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

; ___________________________________________________________________/ }}}3
; ■ ■ True  True ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{3

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

; ___________________________________________________________________/ }}}3

    ; examples from lmulticut_by docstring:
    (assertm = (lmulticut_by oddQ [1 0 1 1 0 0 1] True  True ) [[1 0] [1 1 0 0] [1]])
    (assertm = (lmulticut_by oddQ [1 0 1 1 0 0 1] True  False) [[1 0] [1] [1 0 0] [1]])
    (assertm = (lmulticut_by oddQ [1 0 1 1 0 0 1] False True ) [[0] [0 0]])
    (assertm = (lmulticut_by oddQ [1 0 1 1 0 0 1] False False) [[0] [] [0 0] []])

; ________________________________________________________________________/ }}}2

    (assertm = (count_occurrences 1 [1 2 3 1]) 2)

; _____________________________________________________________________________/ }}}1
; benchmark.hy ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv $F1 "_1.txt")

    (assertm = (type (first (timing apply_n 500 math.sqrt 1E10))) float)
    
    ; (assertm = (dt_print "Timer start") None)
    ; (assertm = (dt_print "Timer +1") None)
    ; (assertm = (dt_print "Timer +2") None)

; _____________________________________________________________________________/ }}}1
; flow.hy       | MACROS: p: fm f> mapm lmapm filter lfilterm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (identity 30) 30)

    (assertm = (flip div 3 4) (div 4 3))
    (assertm = (list (flip map [-1 -2 -3] abs))    [1 2 3])
    (assertm = (list ((pflip map [-1 -2 -3]) abs)) [1 2 3])

    (assertm = (lmap plus [1 2 3] [4 5 6]) [5 7 9])
    (assertm = (lstarmap (fn [x y] (+ x y)) [[1 2] [3 4] [5 6]]) [3 7 11])

    (assertm = (lstarmap (fn [x y] (+ x y)) [[1 2] [3 4] [5 6]]) [3 7 11])
    (assertm = (lreductions plus [1 2 3]) [1 3 6])
    (assertm = (lsums [1 2 3]) [1 3 6])
    (assertm = (product [1 2 3 5]) 30)
    (assertm = (lzip [1 2 3] [4 5 6]) [#(1 4) #(2 5) #(3 6)])

    (assertm = ((nested 3 math.sqrt) 256) (math.sqrt (math.sqrt (math.sqrt 256))))
    (assertm = (apply_n 3 math.sqrt  256) (math.sqrt (math.sqrt (math.sqrt 256))))
    (assertm = (apply_n 1 math.sqrt 256) 16)

; ■ p: ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    (assertm = (lmap (p: (fn [x] x)           ; fn
                         (fm it)              ; fm macro
                         (abs)                ; function
                         (operator.add 4)     ; function
                         str                  ; function
                         (.__contains__ "2")  ; method access
                         .__class__)          ; attribute access
                      [1 2 3])
               (lmap type [True True True]))

; ________________________________________________________________________/ }}}2
; ■ fm, f>, mapm, lmapm, filterm, lfilterm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

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

; ________________________________________________________________________/ }}}2

; _____________________________________________________________________________/ }}}1
; getters.hy ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; nth

    (assertm = (first [1 2]) 1)
    (assertm = (second [1 2 3]) 2)
    (assertm = (third [1 2 3 4]) 3)
    (assertm = (fourth [1 2 3 4 5]) 4)
    (assertm = (beforelast [3 2 1]) 2)
    (assertm = (last [3 2 1]) 1)
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

    ; pluck, lpluck, pluck_attr, lpluck_attr
    ; ncut

    (assertm = (lrange_ 1 3) [1 2 3])
    (assertm = (lrange_ 3 1) [])

    (assertm = (get_ [[1 2] [1 3]] 1 2) 2)
    (assertm = (get_ [[1 2] [1 3]] (- 1 2) 2) 3)
    (assertm gives_error_typeQ (get_ [1 2 3] 0) IndexError)

    (assertm = (nth_ 1  [[1 2] [1 3]]) [1 2])
    (assertm = (nth_ -1 [[1 2] [1 3]]) [1 3])
    (assertm = (nth_ -3 [[1 2] [1 3]]) None)
    (assertm = (nth_ 3 [[1 2] [1 3]]) None)
    (assertm = (nth_ 0 [1 2 3]) None)

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
; IO.hy ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (file_existsQ $F1) True)
    (assertm = (fileQ $F1) True)
    (assertm = (dirQ $F1) False)
    (assertm = (do (write_to_file "hello" $F1) (read_file $F1)) "hello")

; _____________________________________________________________________________/ }}}1
; lens.hy       | MACROS: lns l> l>= &+ &+> ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv idx 3)

    ; lns

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

    ; l> l>= &+ &+>

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
; mathnlogic.hy ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (inc 3.5) 4.5)
    (assertm = (dec 4.5) 3.5)
    (assertm = (sign -4.5) -1)
    (assertm = (neg -4.5) 4.5)
    (assertm = (half -4.5) -2.25)
    (assertm = (double -4.5) -9)
    (assertm = (squared -4.5) 20.25)
    (assertm = (reciprocal -2) -0.5)
    (assertm gives_error_typeQ (sqrt -2) ValueError)

    (assertm = (dist [0 0] [3 4]) 5)
    (assertm = (hypot 3 4) 5)
    (assertm = (normalize [0 4]) [0 1])
    (assertm = (normalize [3 0 0]) [1 0 0])
    (assertm gives_error_typeQ (normalize [0 0 0]) ValueError)
    (setv _norm (py "math.sqrt(1+4+4)"))
    (assertm = (normalize [1 2 2]) (list (map (fn [x] (operator.truediv x _norm)) [1 2 2])))

    (assertm = (exp 0) 1)
    (assertm = (log 1) 0)
    (assertm = (log 8 2) 3)
    (assertm = (ln 1) 0)
    (assertm = (log10 100) 2)
    (assertm = (evenQ 3.5) False)
    (assertm = (oddQ 3.5) False)
    (assertm = (zeroQ 1E-7) False)
    (assertm = (negativeQ -1E-7) True)
    (assertm = (positiveQ -1E-7) False)

    ; trigonometry ...

    (assertm = (minus 3 4) -1)

    ; basic python */+ act as a monoid on numbers, but can't work on 1-arg "str" or []:
    (assertm = (+ 3) 3)
    (assertm = (+)   0)
    (assertm = (* 3) 3)
    (assertm = (*)   1)
    (assertm gives_error_typeQ (+ "pups") TypeError)
    (assertm gives_error_typeQ (+ [1]) TypeError)

    ; same as */+ :
    (assertm = (dadd 3) 3)
    (assertm = (dadd)   0)
    (assertm gives_error_typeQ (dadd "pups") TypeError)
    (assertm gives_error_typeQ (dadd [1]) TypeError)
    (assertm = (dmul 3) 3)
    (assertm = (dmul)   1)

    (assertm = (lmul 3 [1]) [1 1 1])
    (assertm = (smul 3 "a") "aaa")

    ; monoids:

    (assertm = (mul) 1)
    (assertm = (mul 3) 3)
    (assertm = (mul 7 0) 0)
    (assertm = (mul 2 2 3) 12)

    (assertm = (plus) 0)
    (assertm = (plus 3) 3)
    (assertm = (plus 7 0) 7)
    (assertm = (plus 2 2 3) 7)

    ; guest from apl:
    (assertm = (lconcat) [])
    (assertm = (lconcat [1] [] [2]) [1 2])

    (assertm = (sconcat) "")
    (assertm = (sconcat "a" "b" "") "ab")

    ;

    (assertm = (fnot oddQ 2) True)
    (assertm = (fnot eq 1 2) True)
    (assertm = (fnot eq 1 1) False)

    (assertm = (eq_any 1 [1 2 3]) True)
    (assertm = (eq_any 1 [2 3]) False)

    (assertm = (on len eq "123" ["a" 2 "c"]) True)
    (assertm = (on len eq "" []) True)
    (assertm = (on len eq "" "a") False)

    (assertm = (all_fs [evenQ trueQ] True) False)
    (assertm = (any_fs [evenQ trueQ] True) True)
    (assertm = (any_fs [falseQ] False) True)

    (assertm = (oflenQ "123" 3) True)
    (assertm = (oflenQ "123" -3) False)
    (assertm = (oflenQ "" 0) True)
    (assertm = (oflenQ [1 2 3] 3) True)
    (assertm = (oflenQ [1 2 3] -3) False)
    (assertm = (oflenQ [] 0) True)

    (assertm = (zerolenQ "") True)
    (assertm = (zerolenQ []) True)
    (assertm = (zerolenQ "a") False)
    (assertm = (zerolenQ [None]) False)

    ; coice, randint, randfloat, rand01

; _____________________________________________________________________________/ }}}1
; strings.hy ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (strlen "123") 3)

    (assertm = (str_join ["a" "b" "" "c"]) "abc")
    (assertm = (str_join :sep "-" ["a" "b" "" "c"]) "a-b--c")

    (assertm = (strip " bubr ") "bubr")
    (assertm = (strip :chars "|" "Ybubr|") "Ybubr")
    (assertm = (strip :chars "|Ybr" "Ybubr|") "u")
    (assertm = (lstrip " bubr ") "bubr ")
    (assertm = (lstrip :chars "|" "Ybubr|") "Ybubr|")
    (assertm = (lstrip :chars "|Ybr" "Ybubr|") "ubr|")
    (assertm = (rstrip " bubr ") " bubr")
    (assertm = (rstrip :chars "|" "Ybubr|") "Ybubr")
    (assertm = (rstrip :chars "|Ybr" "Ybubr|") "Ybu")

    (assertm = (lowercase "XYZ") "xyz")

; ■ enlengthen ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{2

    (assertm = (enlengthen 3 " ") (* " " 3))
    (assertm = (enlengthen 0 "pups") "pups")
    (assertm = (enlengthen 0 "pups" :on_tail True) "pups")
    (assertm = (enlengthen 0 "pups" :on_tail True :char "123") "pups")
    (assertm = (enlengthen 0 "pups" :on_tail True :char "123" :force_len True) "")
    (assertm = (enlengthen 6 "pups" :char "-") "pups--")
    (assertm = (enlengthen 6 "pups" :char "-" :on_tail False) "--pups")
    (assertm = (enlengthen 4 "pups" :char "-" :on_tail False) "pups")
    (assertm gives_error_typeQ (enlengthen -10 "pups") ValueError)
    (assertm gives_error_typeQ (enlengthen 10 "pups" :char "") ValueError)

    (assertm = (enlengthen 8 "pupos" :force_len True  :on_tail False :char "12") "121pupos")
    (assertm = (enlengthen 8 "pupos" :force_len True  :on_tail True  :char "12") "pupos121")
    (assertm = (enlengthen 8 "pupos" :force_len False :on_tail False :char "12") "121pupos")
    (assertm = (enlengthen 8 "pupos" :force_len False :on_tail True  :char "12") "pupos121")
    (assertm = (enlengthen 3 "pupos" :force_len True  :on_tail False :char "12") "pos")
    (assertm = (enlengthen 3 "pupos" :force_len True  :on_tail True  :char "12") "pup")
    (assertm = (enlengthen 3 "pupos" :force_len False :on_tail False :char "12") "pupos")
    (assertm = (enlengthen 3 "pupos" :force_len False :on_tail True  :char "12") "pupos")

; ________________________________________________________________________/ }}}2

    (assertm = (re_sub   "222" "1" "22232223") "1313")
    (assertm = (re_split "1" "1123123123") ["" "" "23" "23" "23"])
    (assertm = (re_find  "1(23)" "1123123123") "23")
    (assertm = (re_test  "123" "01230") True)
    (assertm = (re_all   "123" "012301230") ["123" "123"])
    (assertm = (re_all   "456" "012301230") [])

; _____________________________________________________________________________/ }}}1
; testing.hy    | MACROS: assertm, gives_error_typeQ | tested by this file directly lol
; typing.hy     | MACROS: f:: ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm = (f:: int -> int => int) (of Callable [int int] int))
    (assertm = (f:: int -> int -> (of Tuple int str)) (of Callable [int int] (of Tuple int str)))

    ; funcs

    (assertm = (noneQ None) True)
    (assertm = (noneQ 1) False)

    (assertm = (notnoneQ None) False)
    (assertm = (notnoneQ 1) True)

    (assertm = (oftypeQ str 1) False)
    (assertm = (oftypeQ int 1) True)
    (assertm = (oftypeQ str "1") True)
    (assertm = (oftypeQ int "1") False)
    (assertm = (oftypeQ type list) True)

    (assertm = (intQ 1.0) False)
    (assertm = (intQ 1) True)

    (assertm = (numberQ 1.00) True)
    (assertm = (numberQ "1") False)

    (assertm = (floatQ 1.00) True)
    (assertm = (floatQ "1") False)

    (assertm = (strQ 1.00) False)
    (assertm = (strQ "1") True)

    (assertm = (dictQ []) False)
    (assertm = (dictQ {"x" 2}) True)

    (assertm = (listQ []) True)
    (assertm = (listQ {"x" 2}) False)
    (assertm = (listQ []) True)
    (assertm = (listQ list) False)
    (assertm = (listQ (list)) True)

    (assertm = (tupleQ #({"x" 2})) True)
    (assertm = (tupleQ {"x" 2}) False)

    (assertm = (setQ #{1 2 3 3}) True)
    (assertm = (setQ {"x" 2}) False)

    (assertm = (iteratorQ (map print [1 2 3])) True)
    (assertm = (iteratorQ (list (map (fn [x] 1) [1 2 3]))) False)

    (assertm = (iterableQ "s") True)
    (assertm = (iterableQ [1 2 3]) True)
    (assertm = (iterableQ 1) False)
    (assertm = (iterableQ {"x" 2}) True)

; _____________________________________________________________________________/ }}}1

