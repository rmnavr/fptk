
    ;; re:
    ;; - match
    ;; - search
    ;; - findall
    ;; - finditer
    ;; - split
    ;; - compile
    ;; - fullmatch
    ;; - escape

    ;; Chars:
    ;; - non-escaped (commands):   .  ^  $  *  +  ? {2,4} [abc]      ( | )
    ;; - escaped (literals):      \. \^ \$ \* \+ \? \{ \} $$_bracket $_parenthesis \| \\ \' \"
    ;; - special:                 \d \D \w \W \s \S \b \B \n \r \f \v
    ;; - raw strings:             r"\d+" = "\\d+"

    ;; flags:
    ;; - re.IGNORECASE or re.I  Makes the pattern matching case-insensitive.
    ;; - re.MULTILINE or re.M   Changes the behavior of ^ and $ to match the start and end of each line within the string, not just the start and end of the entire string.
    ;; - re.DOTALL or re.S      Allows the . character to match any character, including newline characters.
    ;; - re.VERBOSE or re.X     Allows you to write more readable regex by ignoring whitespace and comments.
    ;; - re.ASCII or re.A       Makes the character classes only match ASCII characters (affects \w, \W, \b, and \B).
    ;; - re.LOCALE or re.L      Makes the character classes dependent on the current locale settings (this flag is less commonly used).
    ;;
    ;; usage: (re_sub ... :flags (| re.I re.M))


    (defn dt_print1
        [ #* args
          [fresh_run False]
          [last_T    [None]]
        ]
        (when fresh_run (setv (get last_T 0) None))
        (setv _time_getter hy.I.time.perf_counter)
        (setv curT (_time_getter))
        ;;
        (if (=  (get last_T 0) None)
            (do (setv (get last_T 0) curT)
                (print "[ Timer started ]" #* args))
            (do (setv dT (- curT (get last_T 0)))
                (setv (get last_T 0) curT)
                (print f"[dT = {dT :.6f} s]" #* args))))

    (dt_print1)
    (import fptk [last])
    (dt_print1)
    (require fptk [fm])
    (dt_print1)
