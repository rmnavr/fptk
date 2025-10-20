
; Import and Export ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (export :objects [ inc dec sign neg
                       half double squared reciprocal
                       sqrt dist hypot normalize
                       exp log ln log10
                       evenQ oddQ zeroQ negativeQ positiveQ
                       ;
                       pi sin cos tan degrees radians
                       acos asin atan atan2
                       ;
                       and_ or_ not_ is_ xor
                       eq neq gt lt geq leq
                       matmul div minus
                       dadd dmul lmul smul
                       mul plus sconcat
                       ;
                       fnot eq_any on all_fs any_fs
                       trueQ falseQ oflenQ zerolenQ
                       ;
                       choice randint randfloat rand01
                     ])


    (import functools [reduce])
    (import operator [mul :as operator_mul])

; _____________________________________________________________________________/ }}}1

; [GROUP] Math and logic: Basic math ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import hyrule    [inc])        #_ "inc(n) | = n + 1"
    (import hyrule    [dec])        #_ "dec(n) | = n - 1"
    (import hyrule    [sign])       #_ "sign(n) | will give 0 for n=0"
    (import operator  [neg])        #_ "neg(n) | = -1 * n"

    ;;

    #_ "(half x) = (/ x 2)"
    (defn half       [x] "half(x) = x / 2" (/ x 2))

    #_ "(double x) = (* x 2)"
    (defn double     [x] "double(x) = x * 2" (* x 2))

    #_ "(squared x) = (pow x 2)"
    (defn squared    [x] "squared(x) = pow(x, 2)" (pow x 2))

    #_ "reciprocal(x) = 1/x literally | throws error for x=0"
    (defn reciprocal [x] "reciprocal(x) = 1 / x" (/ 1 x))

    (import math [sqrt])    #_ "sqrt(n) | = √n"
    (import math [dist])    #_ "dist(p, q) -> float | ≈ √((px-qx)² + (py-qy)² ...)"
    (import math [hypot])   #_ "hypot(*coordinates) | = √(x² + y² + ...)" ;;

    #_ "normalize(xs) -> xs | will throw error for zero-len vector"
    (defn normalize [xs]
        " devides each coord of vector to vectors norm,
          example: norm of [1, 2, 3] = sqrt(1 + 4 + 9) = sqrt(14) ~= 3.74,
          so will return [1/3.74, 2/3.74, 3/3.74]
          ---
          will throw error for norm == 0"
        (setv norm (hypot #* xs))
        (if (!= norm 0)
            (return (list (map (fn [%1] (div %1 norm)) xs)))
            (raise (ValueError "Can't normalize zero vector"))))

    (import math [exp]) #_ "exp(x) |"

    (import math [log]) #_ "log(x, base=math.e)" ;;

    #_ "ln(x) = math.log(x, math.e) | coexists with log for clarity"
    (defn ln [x] (log x))

    (import math [log10])  #_ "log10(x) |"

    ;; checks:
    (import funcy [even :as evenQ]) #_ "evenQ(x) |"
    (import funcy [odd  :as oddQ])  #_ "oddQ(x)  |"

    #_ "| checks directly via (= x 0)"
    (defn zeroQ     [x] "checks literally if x == 0" (= x 0))

    #_ "| checks directly via (< x 0)"
    (defn negativeQ [x] "checks literally if x < 0" (< x 0))

    #_ "| checks directly via (> x 0)"
    (defn positiveQ [x] "checks literally if x > 0" (> x 0))

; _____________________________________________________________________________/ }}}1
; [GROUP] Math and logic: Trigonometry ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import math [pi])      #_ "| literally just float pi=3.14..."
    (import math [sin])     #_ "sin(x) | x is in radians"
    (import math [cos])     #_ "cos(x) | x is in radians"
    (import math [tan])     #_ "tan(x) | x is in radians, will give smth like 1.6E+16 for x = pi"
    (import math [degrees]) #_ "degrees(x) | x in radians is converted to degrees"
    (import math [radians]) #_ "radians(x) | x in degrees is converted to radians"
    (import math [acos])    #_ "acos(x) | x is in radians, result is between 0 and pi"
    (import math [asin])    #_ "asin(x) | x is in radians, result is between -pi/2 and pi/2"
    (import math [atan])    #_ "asin(x) | x is in radians, result is between -pi/2 and pi/2"
    (import math [atan2])   #_ "atan2(y, x) | both signs are considered"

; _____________________________________________________________________________/ }}}1
; [GROUP] Math and logic: Base operators to functions ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import operator [and_])                #_ "'and' as function"
    (import operator [or_])                 #_ "'or' as function"
    (import operator [not_])                #_ "'not' as function"
    (import operator [is_])                 #_ "'is' as function"
    (import operator [xor])

    (import operator [eq])                  #_ "equal"
    (import operator [ne :as neq])          #_ "non-equal"
    (import operator [gt])                  #_ "greater than"
    (import operator [lt])                  #_ "less than"
    (import operator [ge :as geq])          #_ "greater or equal"
    (import operator [le :as leq])          #_ "less or equal"

    (import operator [matmul])              #_ "'@' as function"
    (import operator [truediv :as div])     #_ "div(a, b) |"

    #_ "minus(x, y) = x - y |"
    (defn minus [x y] "minux(x, y) = x - y" (- x y))

    ;; =========================================================================
    ;; dunders
    ;; - python behaves like so:
    ;; - (*) = 1, (* 3) = 3
    ;; - (+) = 0, (+ 3) = 3
    ;; - (+ "") = error, (+ []) = error

        #_ "dmul(*args) = arg1 + arg2 + ... | 'dunder mul', '*' operator as a function"
        (defn dmul [#* args]
            "dmul(a1, a2, ...) = a1 * a2 * ...
             dunder mul, '*' operator as a function"
            (* #* args))

        #_ "dadd(*args) = arg1 + arg2 + ... | 'dunder add', '+' operator as a function"
        (defn dadd [#* args]
            "dadd(a1, a2, ...) = a1 + a2 + ...
             dunder add, '+' operator as a function"
            (+ #* args))

    ;; renames

        #_ "lmul(*args) = arg1 * arg2 * ... | rename of * operator, underlines usage for list"
        (defn lmul [#* args]
            "lmul(list, n, ...) = list * n * ...
             rename of * operator, can be used to underline usage on list"
            (* #* args))

        #_ "smul(*args) = arg1 * arg2 * ... | rename of * operator, underlines usage for string"
        (defn smul [#* args]
            " smul(s, n, ...) = s * n * ...
              rename of * operator, can be used to underline usage on string"
            (* #* args))

    ;; monoids

        #_ "mul(*args) | multiplication as a monoid (will not give error when used with 0 or 1 args)"
        (defn mul [#* args]
            " mul(a1, a2, ...) = 1 * a1 * a2 * ...
              multiplication as a monoid with identity = 1,
              can be used with 0 or 1 arg"
            (reduce operator_mul args 1))

        #_ "plus(*args) | addition as a monoid (will not give error when used with 0 or 1 args)"
        (defn plus [#* args]
            " plus(a1, a2, ...) = 0 + a1 + a2 + ...
              addition as a monoid with identity = 0 "
            (reduce (fn [%s1 %s2] (+ %s1 %s2)) args 0))

        #_ "sconcat(*args) | string concantenation as a monoid (will not give error when used with 0 or 1 args)"
        (defn sconcat [#* args]
            " sconcat(s1, s2, ...) = '' + s1 + s2 + ...
              string concantenation as a monoid with identity = '',
              can be used with 0 or 1 args"
            (reduce (fn [%s1 %s2] (+ %s1 %s2)) args ""))

        ;; lconcat (list on itertools.chain) is a monoid on lists too

; _____________________________________________________________________________/ }}}1
; [GROUP] Math and logic: Logic checks ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    #_ "fnot(f, *args, **kwargs) = not(f(*args, **kwargs)) | "
    (defn fnot [f #* args #** kwargs]
        "fnot(f, *args, **kwargs) = not(f(*args, **kwargs))"
        (not (f #* args #** kwargs)))

    #_ "(eq_any x values) | (or (eq x value1) (eq x value2) ...)"
    (defn eq_any [x values]
        "eq_any(x, [v1, v2, ...]) = or(eq(x, v1), eq(x, v2), ...)"
        (or #* (list (map (fn [it] (= x it)) values))))

    #_ "(on f check x y) | (on len eq xs ys) -> (eq (len xs) (len yx))"
    (defn on [f check x y]
        "on(f, check, x, y) = check(f(x), f(y))
         inspired by Haskell's 'on' function"
        (check (f x) (f y)))

    #_ "all_fs(fs, *args, **kwargs) | checks if all f(*args, **kwargs) are True"
    (defn all_fs [fs #* args #** kwargs]
        "all_fs([f1, f2, ...], *args, **kwargs) = and(f1(*args, **kwargs), f2, ...)"
        (and #* (lfor &f fs (&f #* args #** kwargs))))

    #_ "any_fs(fs, *args, **kwargs) | checks if any of f(*args, **kwargs) is True"
    (defn any_fs [fs #* args #** kwargs]
        "all_fs([f1, f2, ...], *args, **kwargs) = or(f1(*args, **kwargs), f2, ...)"
        (or #* (lfor &f fs (&f #* args #** kwargs))))

    #_ "| checks directly via (= x True)"
    (defn trueQ [x] "checks literally if x == True" (= x True))

    #_ "| checks directly via (= x False)"
    (defn falseQ [x] "checks literally if x == False" (= x False))

    #_ "oflenQ(n, xs) -> (= (len xs) n) |"
    (defn oflenQ [n xs] "checks literally if len(xs) == n" (= (len xs) n))

    #_ "| checks literally if (= (len xs) 0)"
    (defn zerolenQ [xs] "checks literally if len(xs) == 0" (= (len xs) 0))

; _____________________________________________________________________________/ }}}1
; [GROUP] Math and logic: Random ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import random    [choice])                 #_ "choice(seq) -> Elem | throws error for empty list"
    (import random    [randint])                #_ "randint(a, b) -> int | returns random integer in range [a, b] including both end points"
    (import random    [uniform :as randfloat])  #_ "randfloat(a, b) -> float | range is [a, b) or [a, b] depending on rounding"
    (import random    [random :as rand01])      #_ "rand01() -> float | generates random number in interval [0, 1) "

    ;; shuffle — is mutating

; _____________________________________________________________________________/ }}}1

