
; Import/Export ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (export :objects [
        ;; filtering:
        lfilter fltr1st filter_split lfilter_split 
        mask_sel lmask_sel mask2idxs idxs2mask
        without lwithout takewhile dropwhile
        bisect_at lbisect_at bisect_by lbisect_by
        ;; iterators/looping: 
        islice lislice inf_range cycle lcycle repeat lrepeat
        concat lconcat cat lcat mapcat lmapcat
        pairwise with_prev with_next
        ;; working with lists:
        flatten lprint lreversed
        partition lpartition partition_by lpartition_by group_by lmulticut_by
        ;; counting:
        count_occurrences
    ])

    (require hyrule [comment of])
    (import  hyrule [dec inc assoc])
    (import  typing [List])
    (import  funcy  [repeat    :as funcy_repeat])
    (import  funcy  [without   :as funcy_without])
    (import  funcy  [lwithout  :as funcy_lwithout])
    (import  funcy  [interpose :as funcy_interpose])
    (import  funcy  [lsplit_at :as funcy_lsplit_at])
    (import  funcy  [partition :as funcy_partition])
    (import  funcy  [chunks    :as funcy_chunks])
    (import  funcy  [last lmap])

; _____________________________________________________________________________/ }}}1

; [GROUP] APL: filtering ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (comment "py | base | filter | filter(function or None, iterable) -> filter object | when f=None, checks if elems are True")
    (import funcy [lfilter]) #_ "lfilter(pred, seq) -> List | funcy list version of extended filter"
    (require fptk._macros [filterm])  #_ "(filterm f xs) | same as filter, but expects fm-syntax for func"
    (require fptk._macros [lfilterm]) #_ "(lfilterm f xs) | list version of lfilterm"

    #_ "fltr1st(f, seq) -> Optional elem | returns first found element (or None)"
    (defn fltr1st [function iterable]
        "returns first found element (via function criteria), returns None if not found"
        (next (gfor &x iterable :if (function &x) &x) None))

    (import funcy [remove  :as reject])   #_ "reject(pred, seq)-> iterator | same as filter, but checks for False"
    (import funcy [lremove :as lreject]) #_ "lreject(pred, seq) -> List | list version of reject"

    #_ "without(items, seq) -> generator | subtracts items from seq (as a sets)"
    (defn without [items seq]
        "returns generator for seq with each item in items removed (does not mutate seq)"
        (funcy_without seq #* items))

    #_ "lwithout(items, seq) -> list | list version of reject"
    (defn lwithout [items seq]
        "returns seq with each item in items removed (does not mutate seq)"
        (funcy_lwithout seq #* items))

    (import funcy [takewhile]) #_ "takewhile([pred, ] seq) | yields elems of seq as long as they pass pred"
    (import funcy [dropwhile]) #_ "dropwhile([pred, ] seq) | mirror of dropwhile"

    (import funcy [split      :as filter_split])  #_ "filter_split(pred, seq) -> passed, rejected |"
    (import funcy [lsplit     :as lfilter_split]) #_ "lfilter_split(pred,seq) -> passed, rejected | list version of filter_split"
    (import funcy [split_at   :as bisect_at])     #_ "bisect_at(n, seq) -> start, tail | len of start will = n, works only with n>=0"

    #_ "lbisect_at(n, seq) -> start, tail | list version of bisect_at, but also for n<0, abs(n) will be len of tail"
    (defn lbisect_at [n seq]
        " splits seq to start and tail lists (returns tuple of lists),
          when n>=0, len of start will be = n (or less, when len(seq) < n),
          when n<0, len of tail will be = n (or less, when len(seq) < abs(n))
        "
        (if (>= n 0)
            (funcy_lsplit_at n seq)
            (funcy_lsplit_at (max 0 (+ (len seq) n)) seq)))

    (import funcy [split_by   :as bisect_by])     #_ " bisect_by(pred, seq) -> taken, dropped | similar to (takewhile, dropwhile)"
    (import funcy [lsplit_by  :as lbisect_by])    #_ "lbisect_by(pred, seq) -> taken, dropped | list version of lbisect"

    ;; MASK SELECTION:

    (import itertools [compress :as mask_sel]) #_ "mask_sel('abc', [1,0,1]) -> iterator: 'a', 'c' | "

    #_ "lmask_sel(data, selectors) -> list |"
    (defn lmask_sel [data selectors]
        "selects by mask: lmask_sel('abc', [1,0,1]) -> ['a', 'c']"
        (list (mask_sel data selectors)))

    #_ "mask2idxs(mask) -> list | mask is list like [1 0 1 0] or [True False True False], which will be converted to [0 2]"
    (defn mask2idxs [mask]
        "mask is list like [1 0 1 0] or [True False True False], which will be converted to [0 2]"
        (setv idxs [])
        (for [[&i &elem] (enumerate mask)]
             (if &elem (idxs.append &i) "no action"))
        (return idxs))

    #_ "idxs2mask(idxs) -> list | idxs is non-sorted list of integers like [0 3 2], which will be converted to [1 0 1 1]"
    (defn idxs2mask [idxs [bools False]]
        " idxs is non-sorted list of positive integers like [0 3 2], which will be converted to [1 0 1 1] ;
          setting bools=True will output [True False True True] instead"
        (when (= (len idxs) 0) (return []))
        ;;
        (setv mask_len (+ 1 (max idxs)))
        (setv mask (list (funcy_repeat 0 mask_len)))
        (for [&idx idxs] (assoc mask &idx 1))
        ;;
        (when bools (setv mask (lmap (fn [it] (= True it)) mask)))
        (return mask))

; _____________________________________________________________________________/ }}}1
; [GROUP] APL: iterators and looping ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import itertools [islice])              #_ "islice(iterable, stop), islice(iterable, start, stop[, step]) | list(islice(inf_range(10), 2)) == [10, 11]"  
    (import itertools [count :as inf_range]) #_ "inf_range(start [, step]) | inf_range(10) -> generator: 10, 11, 12, ..."
    (import itertools [cycle])               #_ "cycle(p) | cycle('AB') -> A B A B ..."
    (import itertools [repeat])              #_ "repeat(elem [, n]) | repeat(10,3) -> 10 10 10" 

    #_ "| list version of islice: lislice"
    (defn lislice [#* kwargs] "literally just list(lislice(...))" (list (islice #* kwargs)))

    #_ "lcycle(p, n) -> list | takes first n elems from cycle(p)"
    (defn lcycle [p n] "takes first n elems from cycle(p)" (lislice (cycle p) n))

    #_ "lrepeat(elem, n) -> list | unlike in repeat, n has to be provided"
    (defn lrepeat [elem n] "literally just list(repeat(elem, n))" (list (repeat elem n)))

    ;; ========================================

    (import itertools [chain :as concat])    #_ "concat(*seqs) -> iterator |"

    #_ "lconcat(*seqs) -> list | list(concat(*seqs))"
    (defn lconcat [#* seqs] "literally just list(concat(*seqs))" (list (concat #* seqs)))

    (import funcy     [cat])        #_ "cat(seqs)  | non-variadic version of concat"
    (import funcy     [lcat])       #_ "lcat(seqs) | non-variadic version of concat"

    (import funcy     [mapcat])     #_ "mapcat(f, *seqs)  | maps, then concatenates"
    (import funcy     [lmapcat])    #_ "lmapcat(f, *seqs) | maps, then concatenates"

    (import funcy     [pairwise])   #_ "pairwise(seq) -> iterator | supposed to be used in loops, will produce no elems for seq with len <= 1"
    (import funcy     [with_prev])  #_ "with_prev(seq, fill=None) -> iterator | supposed to be used in loops"
    (import funcy     [with_next])  #_ "with_next(seq, fill=None) -> iterator | supposed to be used in loops"

; _____________________________________________________________________________/ }}}1
; [GROUP] APL: working with lists ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import hyrule [flatten])   #_ "flattens to the bottom, non-mutating" ;;

    #_ "lprint(seq, sep=None) | prints every elem of seq on new line"
    (defn lprint [seq [sep None]]
          " essentially list(map(print, seq)) ;
            with sep='---' (or some other) will print sep between seq elems
          "
          (if (= sep None)
              (lmap print seq)
              (lmap print (funcy_interpose sep seq)))
          (return None))

    (comment "py | base | reversed | reversed(sequence) -> iterator |") 

    #_ "lreversed(sequence) | list version of reversed"
    (defn lreversed [sequence] (list (reversed sequence)))

    #_ "partition(n, seq, *, step=None, tail=False) -> generator | splits seq to lists of len n, tail=True will allow including fewer than n items"
    (defn partition [n seq * [step None] [tail False]]
        " splits seq to lists of len n,
          at step offsets apart (step=None defaults to n when not given),
          tail=False will allow fewer than n items at the end;
          returns generator"
        (cond (and (not tail) (is step None))
              (funcy_partition n seq)
              (and (not tail) (not (is step None)))
              (funcy_partition n step seq)
              (and tail (is step None))
              (funcy_chunks n seq)
              (and tail (not (is step None)))
              (funcy_chunks n step seq)))

    #_ "lpartition(n, seq, *, step=None, tail=False) -> List | simply list(partition(...))"
    (defn lpartition [n seq * [step None] [tail False]]
        " splits seq to lists of len n,
          at step offsets apart (step=None defaults to n when not given),
          tail=False will allow fewer than n items at the end;
          returns list of lists"
        (list (partition n seq :step step :tail tail)))

    (import funcy [partition_by])   #_ "partition_by(f, seq) -> iterator of iterators | splits when f(item) change" ;;
    (import funcy [lpartition_by])  #_ "lpartition_by(f,seq) -> list of lists | list(partition_by(...))" ;;

    (import funcy [group_by] #_ "group_by(f, seq) -> defaultdict(list) | groups elems of seq keyed by the result of f")

    #_ "lmulticut_by(pred, seq, keep_border=True, merge_border=False) -> list | cut at pred(elem)==True elems"
    (defn #^ (of List list)
        lmulticut_by 
        [ pred
          #^ list seq
          [keep_border  True ]
          [merge_border False]
        ]
        " cuts at elems which give pred(elem)=True
          #
          keep_border =True  will keep elements with pred(elem)=True
          merge_border=True  will cut only at first of a sequence of pred(elem)=True elems 
          #
          in the example below oddQ is function that gives True for odd numbers,
          that is cuts will happen at elems=1
          #
                                                 #  keep_b merge_b
                                                 #  ------ -------
          lmulticut_by(oddQ, [1, 0, 1, 1, 0, 0, 1], True , True ) # -> [[1, 0], [1, 1, 0, 0], [1]]
          lmulticut_by(oddQ, [1, 0, 1, 1, 0, 0, 1], True , False) # -> [[1, 0], [1], [1, 0, 0], [1]]
          lmulticut_by(oddQ, [1, 0, 1, 1, 0, 0, 1], False, True ) # -> [[0], [0, 0]]
          lmulticut_by(oddQ, [1, 0, 1, 1, 0, 0, 1], False, False) # -> [[0], [], [0, 0], []]
        "
        (when (= (len seq) 0) (return []))
        (setv _newLists [])
        (for [&elem seq]
            (if (pred &elem)
                (_newLists.append [&elem])
                (if (= (len _newLists) 0)
                    (_newLists.append [&elem])
                    (. (last _newLists) (append &elem)))))
        (when (not keep_border)
              (setv _newLists (lmap (fn [it] (if (pred (get it 0))
                                                 (cut it 1 None)
                                                 it))
                                    _newLists)))
        (when merge_border
              (if keep_border
                  (do (setv single_borders_pos [])
                      (for [&i (range 0 (len _newLists))]
                           (setv cur_list (get _newLists &i))
                           (when (and (= (len cur_list) 1)
                                      (pred (get cur_list 0)))
                                 (single_borders_pos.append &i)))
                      (for [&i single_borders_pos]
                           (when (< &i (dec (len _newLists)))
                                 (setv (get _newLists (inc &i)) (+ (get _newLists &i) (get _newLists (inc &i))))))
                      (setv others_pos (list (- (set (range 0 (len _newLists))) (set single_borders_pos))))
                      (when (in (dec (len _newLists)) single_borders_pos)
                            (others_pos.append (dec (len _newLists))))
                      (setv _newLists (lfor &n others_pos (get _newLists &n))))
                  (setv _newLists (lwithout [[]] _newLists))))
        (return _newLists))

; _____________________________________________________________________________/ }}}1
; [GROUP] APL: counting ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    #_ "count_occurrences(elem, seq) -> int | rename of list.count method"
    (defn count_occurrences [elem seq] (seq.count elem))

; _____________________________________________________________________________/ }}}1

