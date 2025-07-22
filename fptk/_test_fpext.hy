
    (import  fpext *)
    (require macro *)

; 0-Getters ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm (= (first []) None))
    (assertm (= (second [1]) None))
    (assertm (= (third [1 2]) None))
    (assertm (= (fourth [1 2 3]) None))
    (assertm (= (beforelast [1]) None))
    (assertm (= (last []) None))

    (assertm (= (rest [1 2 3]) [2 3]))
    (assertm (= (rest [1]) []))
    (assertm (= (rest []) []))

    (assertm (= (butlast [1 2 3]) [1 2]))
    (assertm (= (butlast [1]) []))
    (assertm (= (butlast []) []))

    (assertm (= (drop 1 "2") ""))
    (assertm (= (drop -3 "4321") "4"))
    (assertm (= (drop 1 [0]) []))
    (assertm (= (drop 1 [0 1 2]) [1 2]))
    (assertm (= (drop 10 [0 1 2]) []))
    (assertm (= (drop -10 [0 1 2]) []))
    (assertm (= (drop 0 [0 1 2]) [0 1 2]))
    (assertm (= (drop -2 [0 1 2]) [0]))

    (assertm (= (take 3 [1 2 3 4]) [1 2 3]))
    (assertm (= (take 0 []) []))
    (assertm (= (take 0 [1 2 3]) []))
    (assertm (= (take -1 [1]) [1]))
    (assertm (= (take -1 [1 2]) [2]))
    (assertm (= (take -100 [1 2]) [1 2]))

    (assertm (= (pick [0 0 0] [1 2]) [1 1 1]))
    (assertm (= (pick [] [1 2]) []))
    (assertm (= (pick [1 2 3 -1 -2] [0 1 2 3]) [1 2 3 3 2]))

; _____________________________________________________________________________/ }}}1
; 1-Getters ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm (= (get_ [[1 2] [1 3]] 1 2) 2))
    (assertm (= (get_ [[1 2] [1 3]] (- 1 2) 2) 3))

    (assertm (= (nth_ 1  [[1 2] [1 3]]) [1 2]))
    (assertm (= (nth_ -1 [[1 2] [1 3]]) [1 3]))
    (assertm (= (nth_ -3 [[1 2] [1 3]]) None))
    (assertm (= (nth_ 3 [[1 2] [1 3]]) None))

    (assertm (= (get [1 2 3 4 5 6 7] (slice_ 1 3)) [1 2 3]))
    (assertm (= (get [1 2 3 4 5 6 7] (slice_ -3 -1)) [5 6 7]))
    (assertm (= (get [1 2 3 4 5 6 7] (slice_ 6 100)) [6 7]))
    (assertm (= (get [1 2 3 4 5 6 7] (slice_ 1 6 2)) [1 3 5]))
    (assertm (= (get [1 2 3 4 5 6 7] (slice_ -1000 -1)) [1 2 3 4 5 6 7]))

    (assertm (= (cut_ [1 2 3 4 5 6 7] 1 3) [1 2 3]))
    (assertm (= (cut_ [1 2 3 4 5 6 7] -3 -1) [5 6 7]))
    (assertm (= (cut_ [1 2 3 4 5 6 7] 6 100) [6 7]))
    (assertm (= (cut_ [1 2 3 4 5 6 7] 1 6 2) [1 3 5]))
    (assertm (= (cut_ [1 2 3 4 5 6 7] -1000 -1) [1 2 3 4 5 6 7]))

; _____________________________________________________________________________/ }}}1

; FP ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm (= (list (flip map [-1 -2 -3] abs)) [1 2 3]))
    (assertm (= (list ((pflip map [-1 -2 -3]) abs)) [1 2 3]))

    (assertm (= (list (flip map [-1 -2 -3] abs)) [1 2 3]))
    (assertm (= (list ((pflip map [-1 -2 -3]) abs)) [1 2 3]))

    (assertm (= (lstarmap (fn [x y] (+ x y)) [[1 2] [3 4] [5 6]]) [3 7 11]))
    (assertm (= (lzip [1 2 3] [4 5 6]) [#(1 4) #(2 5) #(3 6)]))

    (assertm (= ((nested 3 math.sqrt) 256) (math.sqrt (math.sqrt (math.sqrt 256)))))
    (assertm (= (apply_n 3 math.sqrt  256) (math.sqrt (math.sqrt (math.sqrt 256)))))
    (assertm (= (apply_n 1 math.sqrt 256) 16))

; _____________________________________________________________________________/ }}}1
; APL ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm (= (fltr1st evenQ [1 2 3 4 5]) 2))
    (assertm (= (fltr1st evenQ []) None))
    (assertm (= (list (without [1 2 3] [1 2 3 1 2 5 1 2 3 6])) [5 6]))
    (assertm (= (lwithout [1 2 3] [1 2 3 1 2 5 1 2 3 6]) [5 6]))

    (assertm (= (lbisect_at 3  [1 2 3 4 5 6 7]) (tuple [[1 2 3] [4 5 6 7]])))
    (assertm (= (lbisect_at -2 [1 2 3 4 5 6 7]) (tuple [[1 2 3 4 5] [6 7]])))
    (assertm (= (lbisect_at -10 [1 2 3 4 5 6 7]) (tuple [[] [1 2 3 4 5 6 7]])))
    (assertm (= (lbisect_at 10 [1 2 3 4 5 6 7]) (tuple [[1 2 3 4 5 6 7] []])))
    (assertm (= (lbisect_at 0 [1 2 3 4 5 6 7]) (tuple [[] [1 2 3 4 5 6 7]])))

    (assertm (= (list (partition 10 [1 2 3 4])) []))
    (assertm (= (list (partition 2 [1 2 3 4])) [[1 2] [3 4]]))
    (assertm (= (list (partition 2 :step 1 [1 2 3])) [[1 2] [2 3]]))
    (assertm (= (list (partition 2 :step 3 [1 2 3])) [[1 2]]))
    (assertm (= (list (partition 2 :step 3 [1 2 3 4 5 6])) [[1 2] [4 5]]))
    (assertm (= (list (partition 2 :step 3 :tail True [1 2 3 4])) [[1 2] [4]]))
    (assertm (= (list (partition 2 :tail True [1 2 3 4 5])) [[1 2] [3 4] [5]]))

    (assertm (= (lpartition 10 [1 2 3 4]) []))
    (assertm (= (lpartition 2 [1 2 3 4]) [[1 2] [3 4]]))
    (assertm (= (lpartition 2 :step 1 [1 2 3]) [[1 2] [2 3]]))
    (assertm (= (lpartition 2 :step 3 [1 2 3]) [[1 2]]))
    (assertm (= (lpartition 2 :step 3 [1 2 3 4 5 6]) [[1 2] [4 5]]))
    (assertm (= (lpartition 2 :step 3 :tail True [1 2 3 4]) [[1 2] [4]]))
    (assertm (= (lpartition 2 :tail True [1 2 3 4 5]) [[1 2] [3 4] [5]]))

; _____________________________________________________________________________/ }}}1
; Math ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm (= (normalize [3 0 0]) [1 0 0]))

    (setv _norm (py "math.sqrt(1+4+4)"))
    (assertm (= (normalize [1 2 2]) (list (map (fn [x] (operator.truediv x _norm)) [1 2 2]))))


    (assertm (= (lmul [1] 3) [1 1 1]))
    (assertm (= (smul "a" 3) "aaa"))


    ; monoids:
    (assertm (= (mul) 1))
    (assertm (= (mul 3) 3))
    (assertm (= (mul 7 0) 0))
    (assertm (= (mul 2 2 3) 12))

    (assertm (= (plus) 0))
    (assertm (= (plus 3) 3))
    (assertm (= (plus 7 0) 7))
    (assertm (= (plus 2 2 3) 7))

    (assertm (= (lconcat [1] [] [2]) [1 2]))
    (assertm (= (sconcat "a" "b" "") "ab"))


; _____________________________________________________________________________/ }}}1
; Checks ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1
    
    (assertm (= (zerolenQ "") True))
    (assertm (= (zerolenQ []) True))
    (assertm (= (zerolenQ "a") False))
    (assertm (= (zerolenQ [None]) False))

    (assertm (= (oflenQ "123" 3) True))
    (assertm (= (oflenQ "123" -3) False))
    (assertm (= (oflenQ "" 0) True))
    (assertm (= (oflenQ [1 2 3] 3) True))
    (assertm (= (oflenQ [1 2 3] -3) False))
    (assertm (= (oflenQ [] 0) True))

    (assertm (= (oftypeQ str "1") True))
    (assertm (= (oftypeQ int "1") False))

    (assertm (= (on len eq "123" ["a" 2 "c"]) True))
    (assertm (= (on len eq "" []) True))
    (assertm (= (on len eq "" "a") False))

    (assertm (= (listQ []) True))
    (assertm (= (listQ list) False))
    (assertm (= (listQ (list)) True))

    (assertm (= (fnot eq 1 2) True))
    (assertm (= (fnot eq 1 1) False))

; _____________________________________________________________________________/ }}}1

; Strings ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (assertm (= (str_join ["a" "b" "" "c"]) "abc"))
    (assertm (= (str_join :sep "-" ["a" "b" "" "c"]) "a-b--c"))

    (assertm (= (str_replace "pupsububr" "u" "R") "pRpsRbRbr"))
    (assertm (= (str_replace :count 1 "pupsububr" "u" "R") "pRpsububr"))

    (assertm (= (strip " bubr ") "bubr"))
    (assertm (= (strip :chars "|" "Ybubr|") "Ybubr"))
    (assertm (= (strip :chars "|Ybr" "Ybubr|") "u"))
    (assertm (= (lstrip " bubr ") "bubr "))
    (assertm (= (lstrip :chars "|" "Ybubr|") "Ybubr|"))
    (assertm (= (lstrip :chars "|Ybr" "Ybubr|") "ubr|"))
    (assertm (= (rstrip " bubr ") " bubr"))
    (assertm (= (rstrip :chars "|" "Ybubr|") "Ybubr"))
    (assertm (= (rstrip :chars "|Ybr" "Ybubr|") "Ybu"))

    (assertm (= (enlengthen 3 " ") (* " " 3)))
    (assertm (= (enlengthen 0 "pups") "pups"))
    (assertm (= (enlengthen -10 "pups") "pups"))
    (assertm (= (enlengthen 6 "pups" :char "-") "pups--"))
    (assertm (= (enlengthen 6 "pups" :char "-" :fill_tail False) "--pups"))
    (assertm (= (enlengthen 4 "pups" :char "-" :fill_tail False) "pups"))

; _____________________________________________________________________________/ }}}1

    (print "== TESTING COMPLETED (if there are no errors above, all is good) ==")
