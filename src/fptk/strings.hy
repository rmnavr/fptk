
    (export :objects [ strlen
                       str_join
                       lowercase
                       strip
                       lstrip
                       rstrip
                       enlengthen
                       ; regex:
                       re_sub
                       re_split
                       re_find
                       re_test
                       re_all
                     ])

    (import funcy [str_join :as funcy_str_join])
    (import hyrule [inc])

; [GROUP] Strings: Basics ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    #_ "strlen(text) | rename of len, underlines usage on strings"
    (defn strlen [text]
        "rename of len, underlines usage on strings"
        (len text))

    #_ "str_join(ss, sep='') | rearrangement of funcy.str_join, ss is seq of strings"
    (defn str_join [ss [sep ""]]
        "str_join(['1', '2', '3'], '-') = '1-2-3'"
        (if (bool sep)
            (funcy_str_join sep ss)
            (funcy_str_join ss)))

    #_ "lowercase(string) | str.lower method as a function"
    (defn #^ str lowercase [#^ str string]
        "str.lower method as a function"
        (string.lower))

    #_ "strip(string, chars=None) | str.strip method as a function"
    (defn #^ str strip [#^ str string [chars None]]
        " str.strip method as a function, 
          removes leading and trailing whitespaces (or chars when given)"
        (string.strip chars))

    #_ "lstrip(string, chars=None) | str.lstrip method as a function"
    (defn #^ str lstrip [#^ str string [chars None]]
        "str.lstrip method as a function"
        (string.lstrip chars))

    #_ "rstrip(string, chars=None) | str.rstrip method as a function"
    (defn #^ str rstrip [#^ str string [chars None]]
        "str.rstrip method as a function"
        (string.rstrip chars))

    #_ "enlengthen(string, target_len, char=' ', on_tail=True) | adds char to string until target_len reached"
    (defn #^ str
        enlengthen
        [ #^ int  target_len
          #^ str  string
          #^ str  [char      " "]
          #^ bool [on_tail   True]
          #^ bool [force_len False]
        ]
        " appends char to string until target_len reached

          - if len(string) > target_len, will return string with no change
          - with on_tail=False will prepend chars rather than append
          - with force_len=True will cut string to target_len if required (taking on_tail option into account)
          - when len(char)> 1 is given, repeats it's pattern, but still ensures target_len 
        "
        (when (< target_len 0) (raise (ValueError "target_len < 0 is not allowed")))
        (when (= char "")      (raise (ValueError "empty char is not allowed")))
        ;;
        (when (and force_len 
                   (> (len string) target_len))
              (if on_tail (return (cut string target_len))
                          (return (cut string (- (len string) target_len) (inc (len string))))))
        ;;
        (setv n_required (max 0 (- target_len (len string))))
		(if on_tail
			(setv outp (+ string (cut (* char n_required) 0 (- target_len (len string)))))
			(setv outp (+ (cut (* char n_required) 0 (- target_len (len string))) string)))
		(return outp))

; _____________________________________________________________________________/ }}}1
; [GROUP] Strings: Regex ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    ;; re:
    ;; - match
    ;; - search
    ;; - findall
    ;; - finditer
    ;; - split
    ;; - compile
    ;; - fullmatch
    ;; - escape

    ;; Theory:
    ;;      non-escaped (commands):   .  ^  $  *  +  ? {2,4} [abc]      ( | )
    ;;      escaped (literals):      \. \^ \$ \* \+ \? \{ \} $$_bracket $_parenthesis \| \\ \' \"
    ;;      special:                 \d \D \w \W \s \S \b \B \n \r \f \v
    ;;      raw strings:             r"\d+" = "\\d+"

    (import re        [sub :as re_sub])         #_ "re_sub(rpattern, replacement, string, count=0, flags=0) |"
    (import re        [split :as re_split])     #_ "re_split(rpattern, string) |"
    (import funcy     [re_find])                #_ "re_find(rpattern, string, flags=0) -> str| returns first found"
    (import funcy     [re_test])                #_ "re_test(rpattern, string, ...) -> bool | tests if string has match (not neccessarily whole string)"
    (import funcy     [re_all])                 #_ "re_all(rpattern, string, ...) -> List |"

; _____________________________________________________________________________/ }}}1

