
    (export :objects [ timing dt_print ])

    (import  typing [Tuple Any])
    (require hyrule [of do_n case])

; [GROUP] Benchmarking ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    #_ "timing(f, *args, **kwargs) -> (float, Any) | returns tuple of execution time (in s) and result of f(*args, **kwargs)"
    (defn #^ (of Tuple float Any)
        timing [f #* args #** kwargs]
        "calculated f(*args, **kwargs) and returns tuple: (execution time in s, result)"
        (setv _time_getter hy.I.time.perf_counter)
        (setv t0 (_time_getter))
        (setv outp (f #* args #** kwargs))
        (setv t1 (_time_getter))
        (return #((- t1 t0) outp)))

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

