
; Import and Export ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (export :objects [ nth assoc
                       first second third fourth
                       beforelast last rest butlast
                       drop take pick
                       pluck lpluck pluck_attr lpluck_attr
                       ;
                       range_ lrange_ get_ nth_ slice_ cut_
                     ]
            :macros  [ ncut
                       pluckm    ; fptk macros
                       lpluckm   ; fptk macros
                       getattrm  ; fptk macros
                     ])

    (require hyrule [comment])
    (import  hyrule [dec inc])

; _____________________________________________________________________________/ }}}1

    ;; idxs
    ;; keys
    ;; attrs

; [GROUP] Getters: idxs and keys ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ;; dub basics:

        ;; idxs, keys, attrs:
        (comment "hy     | macro | .     | (. xs [n1] [n2] ...) -> xs[n1][n2]... | throws error when not found")

        ;; idxs, keys:
        (comment "hy     | macro | get   | (get xs n #* keys) -> xs[n][key1]... | throws error when not found")

        ;; idxs:
        (import funcy [nth])        #_ "nth(n, seq) -> Optional elem | 0-based index; works also with dicts"
        (comment "py     | base  | slice | (slice start end step) | returns empty list when not found ")
        (comment "hy     | macro | cut   | (cut xs start end step) -> (get xs (slice start end step)) -> List | returns empty list when none found")

        (import  hyrule [assoc])  #_ "assoc(xs, k1, v1, k2, v2, ...) -> None | ≈ (setv (get xs k1) v1 (get xs k2) v2) ; also possible: (assoc xs :x 1)"
        (require hyrule [ncut])   

    ;; one elem getters:

        (import funcy [first])      #_ "first(seq) -> Optional elem |"
        (import funcy [second])     #_ "second(seq) -> Optional elem |" ;;

        #_ "third(seq) -> Optional elem |"
        (defn third [seq] (if (<= (len seq) 2) (return None) (return (get seq 2))))

        #_ "fourth(seq) -> Optional elem |"
        (defn fourth [seq] (if (<= (len seq) 3) (return None) (return (get seq 3))))

        #_ "beforelast(seq) -> Optional elem |"
        (defn beforelast [seq] (if (<= (len seq) 1) (return None) (return (get seq -2))))

        (import funcy [last])       #_ "last(seq) -> Optional elem |" 

    ;; list getters:

        #_ "rest(seq) -> List | drops 1st elem of list"
        (defn rest [seq] "drops 1st elem of list" (cut seq 1 None))

        #_ "butlast(seq) -> List | drops last elem of list"
        (defn butlast [seq] "drops last elem of list" (cut seq None -1))

        #_ "drop(n, seq) -> List | drops n>=0 elems from start of the list; when n<0, drops from end of the list"
        (defn drop [n seq]
            "drops n>=0 elems from start of seq; when n<0, drops from end of the seq"
            (if (>= n 0) (cut seq n None) (cut seq None n)))

        #_ "take(n, seq) -> List | takes n elems from start; when n<0, takes from end of the list"
        (defn take [n seq]
            "takes n>=0 elems from start of seq; when n<0, takes from end of the seq"
            (if (>= n 0) (cut seq None n) (cut seq (+ (len seq) n) None)))

        #_ "pick(ns, seq) -> List | throws error if some of ns doesn't exist; ns can be list of ints or dict keys"
        (defn pick [ns seq]
            " pics elems ns from seq,
              throws error if some of ns doesn't exist,
              ns can be list of dicts keys
            "
            (lfor &n ns (get seq &n)))

; _____________________________________________________________________________/ }}}1
; [GROUP] Getters: one based index ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import hyrule [thru :as range_])      #_ "range_(start, end=None, step=1) -> List | same as range, but with 1-based index"

    #_ "lrange_(start, end=None, step=1) -> List | list version of range_"
    (defn lrange_ [start [end None] [step 1]]
        (list (range_ start end step)))

    #_ "get_(seq, *ns) -> elem | same as get, but with 1-based index (will throw error for n=0)"
    (defn get_ [seq #* ns]
        " same as hy get macro, but with 1-based index,
          can also work with dict keys,
          will throw error for n=0,
          will throw error if elem not found (just like hy get macro)
        "
        (setv _ns_plus1 
            (lfor &n ns
                (do (when (= &n 0) (raise (IndexError "n=0 can't be used with 1-based getter")))
                    (if (and (= (type &n) int) (>= &n 1))
                        (dec &n)
                        &n)))) ;; this line covers both &n<0 and &n=dict_key        
        (return (get seq #* _ns_plus1)))

    #_ "nth_(n, seq) -> Optional elem | same as nth, but with 1-based index; will return None for n=0"
    (defn nth_ [n seq] 
        " same as nth, but with 1-based index,
          will throw error for n=0,
          will return None if elem not found (just like nth)
        "
        (when (= (type seq) dict) (return (nth n seq)))
        (when (=  n 0) (return None))
        (when (>= n 1) (return (nth (dec n) seq)))
        (return (nth n seq))) ;; this line covers both n<0 and n=dict_key

    #_ "slice_(start, end, step=None) | similar to slice, but with 1-based index; will throw error for start=0 or end=0"
    (defn slice_
        [ start
          end
          [step None]
        ]
        " similar to py slice, but:
          - has 1-based index
          - will throw error when start=0 or end=0
        "
        (cond (>= start 1) (setv _start (dec start))
              (<  start 0) (setv _start start)
              (=  start 0) (raise (IndexError "start=0 can't be used with 1-based getter"))
              True         (raise (IndexError "start in 1-based getter is probably not an integer")))
        ;;
        (cond (=  end -1) (setv _end None)
              (>= end  1) (setv _end end)
              (<  end -1) (setv _end (inc end))
              (=  end  0) (raise (IndexError "end=0 can't be used with 1-based getter"))
              True        (raise (IndexError "end in 1-based getter is probably not an integer")))
        (return (slice _start _end step)))

    #_ "cut_(seq, start, end, step=None) -> List | similar to cut, but with 1-based index; will throw error for start=0 or end=0"
    (defn cut_ [seq start end [step None]]
        " same as hy cut macro, but with 1-based index:
          - will throw error when start=0 or end=0
        "
        (get seq (slice_ start end step)))

; _____________________________________________________________________________/ }}}1
; [GROUP] Getters: keys and attrs ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ;; attrs
    (comment "py | base | getattr | getattr(object, name[, default]) -> value | arg name should be given as str")
    (require fptk._macros [getattrm])   #_ "(getattrm Object 'attr') (getattrm Object .attr) | accepts fptk-style .attr syntax"

    ;; idxs, keys
    (import  funcy  [pluck])            #_ "pluck(key, mappings) -> generator | gets same key (or idx) from every mapping, mappings can be lists of lists/dicts/etc."
    (import  funcy  [lpluck])           #_ "lpluck(key, mappings) -> list | "

    ;; attrs
    (import  funcy  [pluck_attr])       #_ "pluck_attr(attr, objects) -> generator | attr should be given as str" ;;
    (import  funcy  [lpluck_attr])      #_ "lpluck_attr(attr, objects) -> list | list version of pluck_attr" ;;

    ;; idxs, keys and attrs
    (require fptk._macros [pluckm])     #_ "(pluckm n xs) (pluckm key ys) (pluckm .attr zs) | accepts fptk-style .arg syntax"
    (require fptk._macros [lpluckm])    #_ "| list version of pluckm"

; _____________________________________________________________________________/ }}}1

