
    (export :objects [
        with_execution_time
        dt_print
    ])

    (import  typing [Callable Tuple Any])
    (import  re     [sub :as re_sub])
    (import  hyrule [assoc dec])
    (require hyrule [of do_n case])

; [GROUP] Benchmarking ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    #_ "w_e_t(f, *, n=1, tUnit='ns', msg='') -> avrg_time_of_1_run_in_seconds, pretty_string, f_result | f_result is from 1st function execution"
    (defn #^ (of Tuple float Any str)
        with_execution_time
        [ #^ Callable f
          *
          #^ int      [n     1]
          #^ str      [tUnit "ns"]      #_ "s/ms/us/ns"
          #^ str      [msg   ""]
        ]
        " returns tuple:
          - average time of 1 execution in seconds
          - function return value from last execution
          - pretty string of execution time in tUnit units

          tUnit can be: s, ms, us, ns"
        (setv _time_getter hy.I.time.perf_counter)
        (setv n (int n))
        ;;
        (setv t0 (_time_getter))
        (do_n (dec n) (f))
        (setv _outp (f))
        (setv t1 (_time_getter))
        (setv seconds (- t1 t0))
        (setv _time_1_s (/ seconds n))
        ;;
        (case tUnit
            "s"  (do (setv time_n    seconds            ) (setv unit_str " s"))
            "ms" (do (setv time_n (* seconds 1000)      ) (setv unit_str "ms"))
            "us" (do (setv time_n (* seconds 1000000)   ) (setv unit_str "us"))
            "ns" (do (setv time_n (* seconds 1000000000)) (setv unit_str "ns")))
        (setv time_1 (/ time_n n))
        ;;
        (setv line_01       f"/ ({msg})")
        (setv line_02_time1 f"\\ {time_1 :.3f} {unit_str}")
        (setv line_02_n     (re_sub "," "'" f"average of {n :,} runs"))
        (setv line_02_timeN f"test duration: {seconds :.3f} s")
        ;;
        (setv _prompt (+ line_01 "\n"
                         line_02_time1 " as " line_02_n " // " line_02_timeN))
        (return [_time_1_s _outp _prompt]))
    ;;
    ;; (print (execution_time :n 100 (fn [] (get [1 2 3] 1))))

    #_ "dt_printer(* args, fresh_run=False) | starts timer on fresh run, prints time passed since previous call"
    (defn dt_print
        [ #* args
          [fresh_run False]
          [last_T    [None]]
        ]
        " on first run, starts the timer (and print message that it started)
          on subsequent runs prints how many time (in seconds) have passed since previous call
          #
          call with fresh_run = True to reset timer
          #
          last_T should not be touched by user!
          it is used for storing time of previous run between runs"
        (when fresh_run (assoc last_T 0 None))
        (setv _time_getter hy.I.time.perf_counter)
        (setv curT (_time_getter))
        ;;
        (if (=  (get last_T 0) None)
            (do (assoc last_T 0 curT)
                (print "[ Timer started ]" #* args))
            (do (setv dT (- curT (get last_T 0)))
                (assoc last_T 0 curT)
                (print f"[dT = {dT :.6f} s]" #* args))))

; _____________________________________________________________________________/ }}}1

