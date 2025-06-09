
; import ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (require hyrule [of as-> -> ->> doto case branch unless lif do_n list_n ncut])
    (import  fptk *)
    (require fptk [f:: fm p> pluckm lns &+ &+> l> l>=] :readers [L])

; _____________________________________________________________________________/ }}}1

; === Testing ===

; Testing setup ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv _test_macro_FAnnot    False)
    (setv _test_macro_FM        False)
    (setv _test_macroses_Lens   False)
    (setv _test_macro_Pluckm    False)
    (setv _test_macro_pipe      False)

    (defn _test_lines
        [ #^ bool testQ
          #^ str  msg
          #* lines
        ]
        (when testQ
            (print "==" msg "==")
            (lmap (fn [%x] (print ">" %x)) lines)
            (print "")))

; _____________________________________________________________________________/ }}}1

; f:: ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (_test_lines _test_macro_FAnnot "f::"
        (setv #_ DC annotTest (f:: int -> (of List int) -> (of Dict str int) -> (of Optional int) -> (f:: int -> int))) 
        (setv #_ DC annotVariadic (f:: int -> ... -> float))
    )

; _____________________________________________________________________________/ }}}1
; #L ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    #L((abs %1) (- 3))

; _____________________________________________________________________________/ }}}1
; fm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (_test_lines _test_macro_FM "fm"
        ((fm 3))
        ((fm %1) 3)
        ((fm %2) 1 3)
        ((fm (abs 3)))
        ((fm (abs %1)) (- 3))
        ((fm (abs %2)) 1 (- 3))
    )

; _____________________________________________________________________________/ }}}1
; p> ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (_test_lines _test_macro_pipe "p>"
        [ (defclass [dataclass] Point [] (#^ int x) (#^ int y)
                (defn getXscaled [self scale1 scale2] (* scale1 scale2 self.x)))
          (setv ps [(Point 0 1) (Point 2 3)])
          "variables initialized"
        ]
        ;
        (lmap (p> .x (flip minus 10)
                     neg
                     str)
              ps)
        (lmap (p> (.getXscaled #_ "object" 3 4)
                  (flip div 100))
              ps)
        (lmap (p> (nth 1) sqrt neg str) [[0 1] [1 2] [3 4]])
        (lstarmap (p> plus neg str int) [[10 20 30] [1 2 3]])
    )

; _____________________________________________________________________________/ }}}1
; lens: lns, &+, &+>, l>, l>= ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; - [&] combine
    ; - [+] add getter/setter
    ; - [>] apply (requires for nice threading usage (-> macro))
    ; - [l] understands «lns» syntax

    (_test_lines _test_macroses_Lens "lens macroses"
        (do (setv vrbl 3)
            (setv data  (list_n 3 [[1 2 3] [4 5 6] [7 8 9]]))
            (setv data1 [1 2 3])
            "vrbls initialized"
        )
        ;
        (lns 1                      ; -> [1]        // GetitemLens(...)
             vrbl                   ; -> [vrbl]     // GetitemLens(...) // if vrbl not defined, will give Error (this is good)
             (- 1)                  ; -> [(- 1)]    // GetitemLens(...) // only works for Integers! (- vrbl) will translate to (- vrbl), which will most likely give error
             [(- vrbl 1)]           ; no change
             "str"                  ; -> ["str"]    // GetitemLens(...)     
             .attr                  ; -> attr       // GetZoomAttrTraversal('attr')
             (Each))                ; no change
        (lns 1 (mth>   .sort 1))    ; -> (call "f" 1)
        (lns 1 (mut>   .sort 1))    ; -> (call_mut "f" 1)
        (lns 1 (dndr>  / 1))        ; -> (/ (lens) 1)
        (lns 1 (dndr>> / 1))        ; -> (/ 1 (lens))
        ;
        (lns 1 2)                                   ; define UL
        (lns 1 2 (mut> .sort 1 :shallow True))      ; define SF
        (&   (lns 1) (lns 2 (dndr> + 1)))           ; «&» usage #1: composition (ULs + last one can be UL/SF)
        (&   data (lns 1 2 (get)) (lns 2 (get)))    ; «&» usage #2: SFs application (one by one)
        (&+  (lns 1) (lns 2) (set "here"))          ; / compose ULs and SF ...
        (&+> data (lns 1) (lns 2) (mut> .reverse))  ; \ .. and then apply
        (l>  data 1 2 (set "here"))                 ; define SF and apply
        (l>= data1 (Each) (modify math.sqrt))       ; define SF, apply, upd value
        ; same as:
        (. lens [1] [2])                                                    ; define UL
        (. lens [1] [2] (call "sort" 1))                                    ; define SF
        (& (. lens [1]) (+ (. lens [2]) 1))                                 ; «&» usage #1: composition (ULs + last one can be UL/SF)
        (& data (. lens [1] [2] (get)) (. lens [2] (get)))                  ; «&» usage #2: SFs application (one by one)
        (& (. lens [1]) (. lens [2]) (. lens (set "here")))                 ; / compose ULs and SF ...
        ((& (. lens [1]) (. lens [2]) (. lens (call_mut "reverse"))) data)  ; \ .. and then apply
        ((. lens [1] [2] (set "here")) data)                                ; define SF and apply
        (&= data1 (. lens (Each) (modify math.sqrt)))                       ; define SF, apply, upd value
        ;
        ; threading without lens composition:
        (-> data (l> 1 (get)))                  ; RECOMMENDED USAGE
        (-> data (. lens [1] (get)))            ; lens[1].(get)(data)
        (-> data (& (lns 1 (get))))
        (-> data ((lns 1 (get))))               ; requires second wrapping () because «-> macro» would parse: (lns data 1 ...)
        ; threading with lens composition:
        (-> data (&+> (lns 1) (lns 2) (get)))   ; RECOMMENDED USAGE
        (-> data (. (& (lns 1) (lns 2)) (get)))
        ; combination example:
        (&+> (list_n 3 data)
             (lns 1)
             (lns 2 (Fork (& (lns 1) (lns 2))
                          (lns 2)))
             (set "x"))
    )


; _____________________________________________________________________________/ }}}1
; pluckm ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (_test_lines _test_macro_Pluckm "pluckm"
        (do (setv xs [[0 1] [2 3]])
            (setv ds [(dict :x 0 :y 1) (dict :x 2 :y 3)])
            (setv i  0)
            (setv i  0)
            (defclass [dataclass] Point [] (#^ int x) (#^ int y))
            (setv ps [(Point 0 1) (Point 2 3)])
            "vrbls initialized"
        )
        ;
        (pluckm 0       xs)    ; -> (lpluck      0       xs)
        (pluckm i       xs)    ; -> (lpluck      i       xs)
        (pluckm (- 1 1) xs)    ; -> (lpluck      (- 1 1) xs)
        (pluckm "x"     ds)    ; -> (lpluck      "x"     ds)
        (pluckm .x      ps)    ; -> (lpluck_attr "x"     ps)
    )


; _____________________________________________________________________________/ }}}1


; === Usage/Docs ===

; data to work with ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ; for pluckm:
    (setv i 0)
    (setv xs [[1 2 3] [1 2 3] [1 2 3]])
    (setv ds [{"x" 0 "y" 1} {"x" 2 "y" 3}])
    (defclass [dataclass] ForPluck [] (#^ int x) (#^ int y))
    (setv cs [(ForPluck 5 6) (ForPluck 7 8)])

    ; for partial:
    (defclass [dataclass] ForPartial []
        (#^ int x)
        (#^ int y)
        (defn mth [self a b] (* a b self.x)))
    (setv ps [(ForPartial 7 8) (ForPartial 9 10)])

    ; for lens:
    (setv xl [[1 2 3] [1 2 3] [1 2 3]])

; _____________________________________________________________________________/ }}}1
; usage/docs ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (setv funcType (f:: int -> int => (of Tuple int str)))
    (f:: int -> int => (of Tuple int str))

    #L(* %1 2)      ; renaming of #% macro

	(fm (* %1 2))  

    (list (map (p> abs (operator.add 4) str) (range -10 0)))
    (lmap (p> (nth 1) str) [[1 2 3] [1 2 3]])               
    (lmap (p> .x (flip minus 10) str) ps)
    (lmap (p> (.mth 3 4) str) ps)

    (pluckm 0       xs)    ; -> (lpluck      0       xs)
    (pluckm i       xs)    ; -> (lpluck      i       xs)
    (pluckm (- 1 1) xs)    ; -> (lpluck      (- 1 1) xs)
    (pluckm "x"     ds)    ; -> (lpluck      "x"     ds)
    (pluckm .x      cs)    ; -> (lpluck_attr "x"     ps)    ; attr can't be passed as arg, use (pluck_attr "attr" ..) if needed

                  ; / unpacks to [vrbl]
                  ; ↓ 
	(lns 1 "dict" vrbl .attr (Each))                        ; attr   can't be passed as arg, use (GetAttr  "attr") if needed
	(lns 1 (mth> .sort))				                    ; method can't be passed as arg, use (call     "mth" ..) if needed
	(lns 1 (mut> .sort :shallow True))	                    ; method can't be passed as arg, use (call_mut "mth" ..) if needed
	(lns 1 (dndr>  / 1))				
	(lns 1 (dndr>> / 1))                                    
    ;
    (lns 1 (Each) (set 3))                                  ; can define UL/SF
    (l>  xl 1 (Each) (modify sqrt))                         ; define SF and apply
    (l>= xl 1 (Each) (modify sqrt))                         ; define SF, apply, upd value
    (&+  (lns 1) (lns 2) (set "here"))                      ; / compose ULs and «SF-maker-func» ...
    (&+> xl (lns 1) (mut> .reverse))                        ; \ .. and then apply
    ; orig lens lib functionality:
    (&   (lns 1) (lns 1) (lns 2 (dndr> + 1)))               ; #1: composition (ULs + last one can be UL/SF)
    (&   xl (lns 1 (get)) (lns 2 (get)))                    ; #2: SFs application (one after another)

; _____________________________________________________________________________/ }}}1

