
---
fptk functions, macroses and imported modules:
1. You are here -> [Functions and modules](https://github.com/rmnavr/fptk/blob/main/docs/functions.md)
2. [Basic macros](https://github.com/rmnavr/fptk/blob/main/docs/macros.md)
3. [Lens related macros](https://github.com/rmnavr/fptk/blob/main/docs/lens.md)
---

# Auto-generated full list of FPTK entities (except macros)

## Legend

List below has format:
```hy
=== Group name 1 ===
TYPE source_lib | func_or_class_name :: signature ; description
TYPE source_lib | func_or_class_name :: signature ; description

=== Group name 2 ===
...
```

Column `TYPE` shows if things are simple imports/reimports: ...
```hy
FULL MODULE  | sys          ; (import sys)
FROM: math   | ln (<-log)   ; (import math [log :as ln])
MACR: hyrule | of           ; (require hyrule [of])
INFO: hy     | cut /macro/  ; shows info on hy/py functions/macro (which are already always in main context); given just for big picture
```

... or fptk-defined entities:
```hy
SETV: fptk   | StrictNumber ; entity defined internally via (setv ...)
DEFN: fptk   | third        ; entity defined internally via (defn ...)
```

## List of fptk entities

```hy
=== Import Full Modules ===
FULL MODULE           | sys                   ; py base module
FULL MODULE           | os                    ; py base module
FULL MODULE           | math                  ; py base module
FULL MODULE           | operator              ; py base module
FULL MODULE           | random                ; py base module
FULL MODULE           | re                    ; py base module
FULL MODULE           | itertools             ; py base module
FULL MODULE           | functools             ; py base module
FROM: pprint          | pprint
FULL MODULE           | hyrule                ; hy base module
FULL MODULE           | funcy                 ; 3rd party

=== Typing ===
MACR: hyrule          | of
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
FROM: pydantic        | Int (<-StrictInt)
FROM: pydantic        | Str (<-StrictStr)
FROM: pydantic        | Float (<-StrictFloat)
SETV: fptk            | Number                ; Int or Float
FROM: pydantic        | validate_call         ; decorator for type-checking func args
SETV: fptk            | validateF             ; same as validate_call but with option validate_return=True set (so, it validates args + return type)
FROM: returns.result  | Result
FROM: returns.result  | Success
FROM: returns.result  | Failure
FROM: funcy           | isnone
FROM: funcy           | notnone

=== Getters ===
FROM: lenses          | lens
INFO: hy              | . /macro/             :: (. xs [n1] [n2] ...) -> xs[n1][n2]...  ; throws error when not found
INFO: hy              | get /macro/           :: (get xs n #* keys) -> xs[n][key1]...  ; throws error when not found
FROM: funcy           | nth                   :: nth(n, xs) -> Optional elem  ; 0-based index; works also with dicts
INFO: py              | slice /base/          :: (slice start end step)
INFO: hy              | cut /macro/           :: (cut xs start end step) -> (get xs (slice start end step)) -> List  ; gives empty list when none found
FROM: hyrule          | assoc                 :: (assoc xs k1 v1 k2 v2 ...) -> (setv (get xs k1) v1 (get xs k2) v2) -> None  ; also possible: (assoc xs :x 1)
MACR: hyrule          | ncut
FROM: funcy           | first                 :: first(xs) -> Optional elem
FROM: funcy           | second                :: second(xs) -> Optional elem
DEFN: fptk            | third                 :: third(xs) -> Optional elem
DEFN: fptk            | fourth                :: fourth(xs) -> Optional elem
DEFN: fptk            | beforelast            :: beforelast(xs) -> Optional elem
FROM: funcy           | last                  :: last(xs) -> Optional elem
DEFN: fptk            | rest                  :: rest(xs) -> List  ; drops 1st elem of list
DEFN: fptk            | butlast               :: butlast(xs) -> List  ; drops last elem of list
DEFN: fptk            | drop                  :: drop(n, xs) -> List  ; drops n elems from start of the list; when n<0, drops from end of the list
DEFN: fptk            | take                  :: take(n, xs) -> List  ; takes n elems from start; when n<0, takes from end of the list
DEFN: fptk            | pick                  :: pick(ns, xs) -> List  ; throws error if idx doesn't exist; also works with dicts keys
FROM: funcy           | lpluck                :: lpluck(key, xs)  ; works also with dicts
FROM: funcy           | lpluck_attr           :: lpluck_attr(attr_str, xs)

=== index-1-based getters ===
FROM: hyrule          | range_ (<-thru)       :: range_(start, end, step) -> List  ; same as range, but with 1-based index
DEFN: fptk            | get_                  :: get_(xs, n1, n2, ...) -> elem  ; same as get, but with 1-based index (will throw error for n=0)
DEFN: fptk            | nth_                  :: nth_(n, xs) -> Optional elem  ; same as nth, but with 1-based index (will throw error for n=0)
DEFN: fptk            | slice_                :: slice_(start, end, step)  ; same as slice, but with 1-based index (it doesn't understand None and 0 for start and end arguments)
DEFN: fptk            | cut_                  :: cut_(xs, start, end, step) -> List  ; same as cut, but with 1-based index (it doesn't understand None and 0 for start and end arguments)

=== Control flow ===
INFO: hy              | if /base/             :: (if check true false)
INFO: hy              | cond /base/           :: (cond check1 do1 ... true doT)
MACR: hyrule          | case
MACR: hyrule          | branch
MACR: hyrule          | unless
MACR: hyrule          | lif

=== Compositions ===
FROM: hyrule          | constantly            ; (setv answer (constantly 42)) (answer 1 :x 2) -> 42
FROM: funcy           | identity              ; identity(30) -> 30
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
FROM: funcy           | ljuxt                 :: ljuxt(f,g,...) = [f, g] applicator
DEFN: fptk            | flip                  :: flip(f, a, b) = f(b, a)  ; example: (flip lmap [1 2 3] sqrt)
DEFN: fptk            | pflip                 :: pflip(f, a) = f(_, a) partial applicator  ; example: (lmap (pflip div 0.1) (thru 1 3))

=== APL: n-applicators ===
MACR: hyrule          | do_n                  :: (do_n   n #* body) -> None
MACR: hyrule          | list_n                :: (list_n n #* body) -> List
DEFN: fptk            | nest                  :: nest(n, f)  ; f(f(f(...f)))
DEFN: fptk            | apply_n               :: apply_n(n, f, *args, **kwargs)  ; f(f(f(...f(*args, **kwargs))

=== APL: Threading ===
INFO: py              | map /base/            :: map(f, *xss) -> iterator
FROM: funcy           | lmap                  ; lmap(f, *xss) -> List
FROM: itertools       | starmap               ; starmap(f, xs)
DEFN: fptk            | lstarmap              :: lstarmap(f,xs) -> List
FROM: functools       | reduce                :: reduce(f, xs[, x0]) -> value  ; reduce + monoid = binary-function for free becomes n-arg-function
INFO: py              | zip /base/            :: zip(*xss) -> iterator I guess
DEFN: fptk            | lzip                  :: lzip(*xss) = list(zip(*xss))

=== APL: Filtering ===
INFO: py              | filter /base/         :: filter(f or None, xs) -> filter object  ; when f=None, checks if elems are True
FROM: funcy           | lfilter               ; lfilter(f, xs) -> List
DEFN: fptk            | fltr1st               :: fltr1st(f, xs) -> Optional elem  ; returns first found element (or None)
DEFN: fptk            | count_occurrences     :: count_occurrences(elem, xs) -> int  ; rename of list.count method

=== APL: Work on lists ===
FROM: hyrule          | flatten               ; flattens to the bottom
DEFN: fptk            | lprint                :: lprint(lst) -> (lmap print lst)
INFO: py              | reversed /base/       :: reversed(xs) -> iterator
DEFN: fptk            | lreversed             :: lreversed(*args) = list(reversed(*args))

=== General Math ===
FROM: hyrule          | inc
FROM: hyrule          | dec
FROM: hyrule          | sign
FROM: operator        | neg                   ; (half x) = (/ x 2)
DEFN: fptk            | half
DEFN: fptk            | double                ; (double x) = (* x 2)
DEFN: fptk            | squared               ; (squared x) = (pow x 2)
DEFN: fptk            | reciprocal            :: reciprocal(x) = 1/x literally
FROM: math            | sqrt
FROM: math            | dist                  :: dist(v1, v2) -> float  ; ≈ √((v1x-v2x)² + (v1y-v2y)² ...)
FROM: math            | hypot                 :: hypot(x, y, ...)  ; = √(x² + y² + ...)
DEFN: fptk            | normalize             :: normalize(vector) -> vector  ; returns same vector if it's norm=0
FROM: operator        | div (<-truediv)
FROM: math            | product (<-prod)
FROM: math            | exp
FROM: math            | log                   ; log(x, base=math.e)
DEFN: fptk            | ln                    :: ln(x) = math.log(x, math.e)  ; coexists with log for clarity
FROM: math            | log10

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
FROM: math            | atan2                 :: atan2(y, x) -> value  ; both signs are considered
FROM: math            | sin

=== Base operators to functions ===
DEFN: fptk            | minus                 :: minus(x, y) = x - y
DEFN: fptk            | mul                   :: mul(*args)  ; multiplication as a monoid (will not give error when used with 0 or 1 args)
DEFN: fptk            | lmul                  :: lmul(*args) = arg1 * arg2 * ...  ; rename of * operator, underlines usage for list
DEFN: fptk            | smul                  :: smul(*args) = arg1 * arg2 * ...  ; rename of * operator, underlines usage for string
DEFN: fptk            | plus                  :: plus(*args)  ; addition as a monoid (will not give error when used with 0 or 1 args)
DEFN: fptk            | sconcat               :: sconcat(*args)  ; string concantenation as a monoid (will not give error when used with 0 or 1 args)
DEFN: fptk            | lconcat               :: lconcat(*args)  ; list concantenation as a monoid (will not give error when used with 0 or 1 args)

=== Logic and ChecksQ ===
FROM: hyrule          | xor
FROM: operator        | eq                    ; equal
FROM: operator        | neq (<-ne)            ; non-equal
FROM: funcy           | evenQ (<-even)
FROM: funcy           | oddQ (<-odd)
DEFN: fptk            | zeroQ                 ; checks directly via (= x 0)
DEFN: fptk            | negativeQ             ; checks directly via (< x 0)
DEFN: fptk            | positiveQ             ; checks directly via (> x 0)
DEFN: fptk            | zerolenQ              ; checks literally if (= (len xs) 0)
DEFN: fptk            | oftypeQ               :: (oftypeQ tp x) -> (= (type x) tp)
DEFN: fptk            | oflenQ                :: (oflenQ xs n) -> (= (len xs) n)
DEFN: fptk            | on                    :: (on f check x y #* args)  ; (on len eq xs ys zs) -> checks if len of xs/ys/zs is the same, check has to be func of 2+ args
DEFN: fptk            | intQ
DEFN: fptk            | floatQ
DEFN: fptk            | dictQ
FROM: funcy           | listQ (<-is_list)     :: listQ(seq)  ; checks if seq is list
FROM: funcy           | tupleQ (<-is_tuple)   :: tupleQ(seq)  ; checks if seq is tuple
DEFN: fptk            | fnot                  :: fnot(f, *args, **kwargs) = not(f(*args, **kwargs))

=== Strings ===
DEFN: fptk            | strlen                :: strlen(text)  ; rename of len, underlines usage on strings
DEFN: fptk            | str_join              :: str_join(seq, sep='')  ; rearrangement of funcy.str_join
DEFN: fptk            | str_replace           ; str_replace(string, old, new, count=-1) ; rename of string.replace method
DEFN: fptk            | lowercase
DEFN: fptk            | endswith              ; endswith(string, ending) -> bool ; rename of string.endswith method
DEFN: fptk            | strip
DEFN: fptk            | lstrip
DEFN: fptk            | rstrip
DEFN: fptk            | pad_string            :: pad_string(string, required_len, fill_char=' ', pad_right=False  ; by default adds new symbols to the right

=== Regex ===
FROM: re              | re_sub (<-sub)        :: re_sub(rpattern, replacement, string, count=0, flags=0)
FROM: re              | re_split (<-split)    :: re_split(rpattern, string)
FROM: funcy           | re_find               :: re_find(rpattern, string, flags=0) -> str  ; returns first found
FROM: funcy           | re_test               :: re_test(rpattern, string, ...) -> bool  ; tests if string has match (not neccessarily whole string)
FROM: funcy           | re_all                :: re_all(rpattern, string, ...) -> List

=== Random ===
FROM: random          | choice                :: choice(xs) -> Elem  ; throws error for empty list
FROM: random          | randint               :: randint(a, b) -> int  ; returns random integer in range [a, b] including both end points
FROM: random          | randfloat (<-uniform) :: randfloat(a, b) -> float  ; range is [a, b) or [a, b] depending on rounding
FROM: random          | rand01 (<-random)     :: rand01() -> float  ; generates random number in interval [0, 1)

=== IO ===
FROM: os.path         | file_existsQ (<-exists) :: file_existsQ(filename)  ; also works on folders
DEFN: fptk            | read_file             :: read_file(file_name, encoding='utf-8') -> str  ; reads whole file content
DEFN: fptk            | write_file            :: write_file(text, file_name, mode='w', encoding='utf-8')  ; modes: 'w' - (over)write, 'a' - append, 'x' - exclusive creation

=== Benchmarking ===
DEFN: fptk            | with_execution_time   :: w_e_t(f, n=1, tUnit='ns', msg='') -> avrg_time_of_1_run_in_seconds, pretty_string, f_result  ; f_result is from 1st function execution
DEFN: fptk            | dt_print              ; use dt_printer('msg1', 'msg2', ...) normally, use dt_printer(fresh_run=True, 'msg1', ...) to reset timer
```