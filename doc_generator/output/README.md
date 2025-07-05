# Auto-generated full list of FPTK entities

```hy
=== Import Full Modules ===
FULL MODULE           | sys
FULL MODULE           | math
FULL MODULE           | operator
FULL MODULE           | random
FULL MODULE           | re
FULL MODULE           | itertools
FULL MODULE           | functools
FROM: pprint          | pprint
FULL MODULE           | hyrule
FULL MODULE           | funcy

=== Typing ===
MACR: hyrule          | of
MACR: hyrule          | comment
FROM: dataclasses     | dataclass
FROM: enum            | Enum
FROM: abc             | ABC
FROM: abc             | abstractmethod
FROM: typing          | List
FROM: typing          | Tuple
FROM: typing          | TypedDict
FROM: typing          | Dict
FROM: typing          | Union
FROM: typing          | Generator
FROM: typing          | Any
FROM: typing          | Optional
FROM: typing          | Callable
FROM: typing          | Literal
FROM: typing          | Type
FROM: pydantic        | BaseModel
FROM: pydantic        | StrictInt
FROM: pydantic        | StrictStr
FROM: pydantic        | StrictFloat
FROM: pydantic        | validate_args (<-validate_arguments)
SETV: fptk            | StrictNumber          ; Int or Float
FROM: returns.result  | Result
FROM: returns.result  | Success
FROM: returns.result  | Failure
FROM: funcy           | isnone
FROM: funcy           | notnone

=== Getters ===
FROM: lenses          | lens
FROM: funcy           | first
FROM: funcy           | second
FROM: funcy           | last
FROM: funcy           | nth                   :: nth(n, xs)  ; works also with dicts; zero-based index btw
FROM: funcy           | lpluck                :: lpluck(key, xs)  ; works also with dicts
FROM: funcy           | lpluck_attr
INFO: hyrule          | get /macro/           :: get(xs,n)
FROM: hyrule          | assoc
MACR: hyrule          | ncut                  :: third(xs) -> Optional elem
DEFN: fptk            | third
DEFN: fptk            | fourth                :: third(xs) -> Optional elem
DEFN: fptk            | beforelast            :: beforelast(xs) -> Optional elem
DEFN: fptk            | rest                  :: rest(xs) -> List  ; [xx 2 3 4 5]
DEFN: fptk            | butlast               :: rest(xs) -> List  ; [1 2 3 4 xx]
DEFN: fptk            | drop                  :: drop(n, xs) -> List  ; drops from start/end of the list
DEFN: fptk            | take                  :: take(n, xs) -> List  ; takes from start/end of the list
DEFN: fptk            | pick                  :: pick(ns, xs) -> List  ; throws error if idx doesn't exist; also works with dicts keys

=== APL ===
INFO: hy              | if /base/             :: (if check true false)
INFO: hy              | cond /base/           :: (cond check1 do1 ... true doT)
MACR: hyrule          | case
MACR: hyrule          | branch
MACR: hyrule          | unless
MACR: hyrule          | lif
FROM: functools       | reduce                ; THEORY: reduce + monoid = 2-arg-function for free becomes n-arg-function
FROM: itertools       | starmap
FROM: funcy           | lmap
DEFN: fptk            | lstarmap              :: lstarmap(f,xs) -> List
INFO: py              | filter /base/         :: filter(f, xs) -> List
FROM: funcy           | lfilter               :: fltr1st(f, xs) -> Optional elem  ; returns first found element
DEFN: fptk            | fltr1st
DEFN: fptk            | count_occurrences     :: count_occurrences(elem, xs) -> int
FROM: itertools       | cycle
FROM: hyrule          | thru
FROM: hyrule          | flatten               ; flattens to the bottom
DEFN: fptk            | lprint
DEFN: fptk            | lzip                  :: lzip(*args) = list(zip(*args))
DEFN: fptk            | lreversed             :: lreversed(*args) = list(reversed(*args))

=== Compositions ===
FROM: hyrule          | constantly
FROM: funcy           | identity
MACR: hyrule          | do_n
MACR: hyrule          | list_n
DEFN: fptk            | apply_n               :: apply_n(f, n, arg)  ; f(f(f(...f(arg))
FROM: funcy           | ljuxt
MACR: hyrule          | as->
MACR: hyrule          | ->
MACR: hyrule          | ->>
MACR: hyrule          | doto
FROM: funcy           | curry
FROM: funcy           | autocurry
FROM: funcy           | partial
FROM: funcy           | rpartial
FROM: funcy           | compose
FROM: funcy           | rcompose
DEFN: fptk            | flip                  :: flip(f, a, b) = f(b, a)  ; example: (flip lmap [1 2 3] sqrt)
DEFN: fptk            | pflip                 :: pflip(f, a) = f(_, a) partial applicator  ; example: (lmap (pflip div 0.1) (thru 1 3))

=== General Math ===
FROM: hyrule          | inc
FROM: hyrule          | dec
FROM: hyrule          | sign
FROM: operator        | neg
DEFN: fptk            | half
DEFN: fptk            | double
DEFN: fptk            | reciprocal            :: reciprocal(x) = 1/x literally
FROM: math            | sqrt
FROM: math            | dist                  :: dist(v1, v2) -> float  ; ≈ √((v1x-v2x)² + (v1y-v2y)² ...)
FROM: math            | hypot                 :: hypot(x, y, ...)  ; = √(x² + y² + ...)
DEFN: fptk            | normalize             :: normalize(vector) -> vector  ; returns same vector if it's norm=0
FROM: operator        | div (<-truediv)
FROM: math            | product (<-prod)
FROM: math            | log                   ; log(x, base=math.e)
FROM: math            | exp
FROM: math            | log10
DEFN: fptk            | ln                    :: ln(x) = math.log(x, math.e)  ; coexists with log for clarity

=== Trigonometry ===
FROM: math            | pi
FROM: math            | sin
FROM: math            | cos
FROM: math            | tan
FROM: math            | degrees
FROM: math            | radians
FROM: math            | acos
FROM: math            | asin
FROM: math            | atan
FROM: math            | atan2
FROM: math            | sin

=== Funcs from base operators ===
DEFN: fptk            | minus                 ; minus(x, y) = x - y
DEFN: fptk            | mul                   :: mul(*args) = arg1 * arg2 * ...  ; rename of * operator, underlines usage for numbers
DEFN: fptk            | lmul                  :: lmul(*args) = arg1 * arg2 * ...  ; rename of * operator, underlines usage for strings
DEFN: fptk            | smul                  :: smul(*args) = arg1 * arg2 * ...  ; rename of * operator, underlines usage for lists
DEFN: fptk            | plus                  :: plus(*args) = arg1 + arg2 + ...  ; rename of + operator, underlines usage for numbers
DEFN: fptk            | sconcat               :: sconcat(*args) = arg1 + arg2 + ...  ; rename of + operator, underlines usage for strings
DEFN: fptk            | lconcat               :: lconcat(*args) = arg1 + arg2 + ...  ; rename of + operator, underlines usage for lists

=== Logic and ChecksQ ===
FROM: hyrule          | xor
FROM: operator        | eq                    ; equal
FROM: operator        | neq (<-ne)            ; non-equal
FROM: funcy           | even
FROM: funcy           | odd
DEFN: fptk            | not_                  :: not_(f, *args, **kwargs) = not(f(*args, **kwargs))
DEFN: fptk            | zeroQ                 ; checks directly via (= x 0)
DEFN: fptk            | negativeQ             ; checks directly via (< x 0)
DEFN: fptk            | positiveQ             ; checks directly via (> x 0)

=== Strings ===
DEFN: fptk            | str_join              :: str_join(seq, sep='')  ; rearrangement of funcy.str_join
DEFN: fptk            | str_replace           ; str_replace(string, old, new, count=-1) ; rename of string.replace method
DEFN: fptk            | lowercase
DEFN: fptk            | endswith              ; endswith(string, ending) -> bool ; rename of string.endswith method
DEFN: fptk            | strip
DEFN: fptk            | lstrip
DEFN: fptk            | rstrip

=== Regex ===
FROM: re              | re_sub (<-sub)        :: re_sub(rpattern, replacement, string, count=0, flags=0)
FROM: re              | re_split (<-split)    :: re_split(rpattern, string)
FROM: funcy           | re_find               :: re_find(rpattern, string, flags=0) -> str  ; returns first found
FROM: funcy           | re_test               :: re_test(rpattern, string, ...) -> bool  ; tests string has match (not neccessarily whole string)
FROM: funcy           | re_all                :: re_all(rpattern, string, ...) -> List

=== Random ===
FROM: random          | choice                :: choice(xs) -> Elem  ; throws error for empty list
FROM: random          | randint               :: randint(a, b) -> int  ; returns random integer in range [a, b] including both end points
FROM: random          | randfloat (<-uniform) :: randfloat(a, b) -> float  ; range is [a, b) or [a, b] depending on rounding
FROM: random          | rand01 (<-random)     ; rand01() -> float in interval [0, 1)

=== with_execution_time ===
DEFN: fptk            | with_execution_time   :: wet(f, n=1, tUnit='ns', msg='') -> time_of_1_exec_in_seconds, pretty_string, f_result  ; result is from 1st function execution
```
