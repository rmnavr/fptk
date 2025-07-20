
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
FULL MODULE           | funcy                 ; 3rd party module (FP related)
FROM: lenses          | lens                  ; 3rd party module (for working with immutable structures)

=== Typing ===
MACR: hyrule          | of                    ; (of List int) -> List[int]
FROM: dataclasses     | dataclass
FROM: enum            | Enum
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
FROM: pydantic        | Int (<-StrictInt)     ; will be still of int type, but will perform strict typecheck when variable is created
FROM: pydantic        | Str (<-StrictStr)     ; will be still of str type, but will perform strict typecheck when variable is created
FROM: pydantic        | Float (<-StrictFloat) ; will be still of float type, but will perform strict typecheck when variable is created
SETV: fptk            | Number                ; Union of Int and Float
FROM: pydantic        | validate_call         ; decorator for type-checking func args
SETV: fptk            | validateF             ; same as validate_call but with option validate_return=True set (thus validating args and return type)
FROM: returns.result  | Result                ; Result monad
FROM: returns.result  | Success               ; One of Result monad constructor
FROM: returns.result  | Failure               ; One of Result monad constructor

=== Getters ===
INFO: hy              | . /macro/             :: (. xs [n1] [n2] ...) -> xs[n1][n2]...  ; throws error when not found
INFO: hy              | get /macro/           :: (get xs n #* keys) -> xs[n][key1]...  ; throws error when not found
FROM: funcy           | nth                   :: nth(n, seq) -> Optional elem  ; 0-based index; works also with dicts
INFO: py              | slice /base/          :: (slice start end step)
INFO: hy              | cut /macro/           :: (cut xs start end step) -> (get xs (slice start end step)) -> List  ; gives empty list when none found
FROM: hyrule          | assoc                 :: (assoc xs k1 v1 k2 v2 ...) -> (setv (get xs k1) v1 (get xs k2) v2) -> None  ; also possible: (assoc xs :x 1)
MACR: hyrule          | ncut
FROM: funcy           | first                 :: first(seq) -> Optional elem
FROM: funcy           | second                :: second(seq) -> Optional elem
DEFN: fptk            | third                 :: third(seq) -> Optional elem
DEFN: fptk            | fourth                :: fourth(seq) -> Optional elem
DEFN: fptk            | beforelast            :: beforelast(seq) -> Optional elem
FROM: funcy           | last                  :: last(seq) -> Optional elem
DEFN: fptk            | rest                  :: rest(seq) -> List  ; drops 1st elem of list
DEFN: fptk            | butlast               :: butlast(seq) -> List  ; drops last elem of list
DEFN: fptk            | drop                  :: drop(n, seq) -> List  ; drops n>=0 elems from start of the list; when n<0, drops from end of the list
DEFN: fptk            | take                  :: take(n, seq) -> List  ; takes n elems from start; when n<0, takes from end of the list
DEFN: fptk            | pick                  :: pick(ns, seq) -> List  ; throws error if some of ns doesn't exist; ns can be list of ints or dict keys
FROM: funcy           | lpluck                :: lpluck(key, mappings)  ; gets same key from every mapping, mappings can be list of lists, list of dicts, etc.
FROM: funcy           | lpluck_attr           :: lpluck_attr(attr, objects)

=== index-1-based getters ===
FROM: hyrule          | range_ (<-thru)       :: range_(start, end, step) -> List  ; same as range, but with 1-based index
DEFN: fptk            | get_                  :: get_(xs, *ns) -> elem  ; same as get, but with 1-based index (will throw error for n=0)
DEFN: fptk            | nth_                  :: nth_(n, seq) -> Optional elem  ; same as nth, but with 1-based index (will throw error for n=0)
DEFN: fptk            | slice_                :: slice_(start, end, step)  ; similar to slice, but with 1-based index (also it doesn't understand None and 0 for start and end arguments)
DEFN: fptk            | cut_                  :: cut_(seq, start, end, step) -> List  ; same as cut, but with 1-based index (it doesn't understand None and 0 for start and end arguments)

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
FROM: funcy           | ljuxt                 :: ljuxt(*fs) = [f1, f2, ...] applicator
DEFN: fptk            | flip                  :: flip(f, a, b) = f(b, a)  ; example: (flip lmap [1 2 3] sqrt)
DEFN: fptk            | pflip                 :: pflip(f, a)  ; partial applicator with flipped args, works like: pflip(f, a)(b) = f(b, a), example: (lmap (pflip div 0.1) (thru 1 3))

=== APL: n-applicators ===
MACR: hyrule          | do_n                  :: (do_n   n #* body) -> None
MACR: hyrule          | list_n                :: (list_n n #* body) -> List
DEFN: fptk            | nest                  :: nest(n, f)  ; f(f(f(...f))), returns function
DEFN: fptk            | apply_n               :: apply_n(n, f, *args, **kwargs)  ; f(f(f(...f(*args, **kwargs))

=== APL: Threading ===
INFO: py              | map /base/            ; usage: map(func, *iterables) -> map object
FROM: funcy           | lmap                  :: lmap(f, *seqs) -> List
FROM: itertools       | starmap               ; starmap from itertools; usage: starmap(f, seq)
DEFN: fptk            | lstarmap              :: lstarmap(f, *seqs) -> List  ; literally just list(starmap(f, *seqs))
FROM: functools       | reduce                :: reduce(function, sequence[, initial]) -> value  ; theory: reduce + monoid = binary-function for free becomes n-arg-function
INFO: py              | zip /base/            :: zip(*iterables) -> zip object
DEFN: fptk            | lzip                  :: lzip(*iterables) -> List  ; literally just list(zip(*iterables))

=== APL: Filtering ===
INFO: py              | filter /base/         :: filter(f or None, xs) -> filter object  ; when f=None, checks if elems are True
FROM: funcy           | lfilter               :: lfilter(pred, seq) -> List
DEFN: fptk            | fltr1st               :: fltr1st(f, seq) -> Optional elem  ; returns first found element (or None)
DEFN: fptk            | count_occurrences     :: count_occurrences(elem, seq) -> int  ; rename of list.count method

=== APL: Work on lists ===
FROM: hyrule          | flatten               ; flattens to the bottom
DEFN: fptk            | lprint                :: lprint(seq)  ; literally just list(map(print,seq))
INFO: py              | reversed /base/       :: reversed(sequence) -> iterator
DEFN: fptk            | lreversed             :: lreversed(seq) = list(reversed(seq))

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
FROM: math            | dist                  :: dist(p, q) -> float  ; ≈ √((px-qx)² + (py-qy)² ...)
FROM: math            | hypot                 :: hypot(*coordinates)  ; = √(x² + y² + ...)
DEFN: fptk            | normalize             :: normalize(xs) -> xs  ; returns same vector xs if it's norm=0
FROM: operator        | div (<-truediv)       :: div(a, b)
FROM: math            | product (<-prod)      :: product(iterable, /, *, start=1)  ; product([2, 3, 5]) = 30
FROM: math            | exp                   :: exp(x)
FROM: math            | log                   ; log(x, base=math.e)
DEFN: fptk            | ln                    :: ln(x) = math.log(x, math.e)  ; coexists with log for clarity
FROM: math            | log10                 :: log10(x)

=== Trigonometry ===
FROM: math            | pi                    ; literally just float pi=3.14...
FROM: math            | sin                   :: sin(x)  ; x is in radians
FROM: math            | cos                   :: cos(x)  ; x is in radians
FROM: math            | tan                   :: tan(x)  ; x is in radians, will give smth like 1.6E+16 for x = pi
FROM: math            | degrees               :: degrees(x)  ; x in radians is converted to degrees
FROM: math            | radians               :: radians(x)  ; x in degrees is converted to radians
FROM: math            | acos                  :: acos(x)  ; x is in radians, result is between 0 and pi
FROM: math            | asin                  :: asin(x)  ; x is in radians, result is between -pi/2 and pi/2
FROM: math            | atan                  :: asin(x)  ; x is in radians, result is between -pi/2 and pi/2
FROM: math            | atan2                 :: atan2(y, x)  ; both signs are considered

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
FROM: funcy           | noneQ (<-isnone)
FROM: funcy           | notnoneQ (<-notnone)
DEFN: fptk            | zeroQ                 ; checks directly via (= x 0)
DEFN: fptk            | negativeQ             ; checks directly via (< x 0)
DEFN: fptk            | positiveQ             ; checks directly via (> x 0)
DEFN: fptk            | zerolenQ              ; checks literally if (= (len xs) 0)
DEFN: fptk            | oftypeQ               :: (oftypeQ tp x) -> (= (type x) tp)
DEFN: fptk            | oflenQ                :: (oflenQ xs n) -> (= (len xs) n)
DEFN: fptk            | on                    :: (on f check x y #* args)  ; (on len eq xs ys zs) -> checks if len of xs/ys/zs is the same, check has to be func of 2+ args
DEFN: fptk            | intQ                  :: intQ(x)  ; checks literally if type(x) == int, will also work with StrictInt from pydantic
DEFN: fptk            | floatQ                :: floatQ(x)  ; checks literally if type(x) == float, will also work with StrictFloat from pydantic
DEFN: fptk            | numberQ               :: numberQ(x)  ; checks for intQ or floatQ, will also work with StrictInt/StrictFloat from pydantic
DEFN: fptk            | dictQ                 :: dictQ(x)  ; checks literally if type(x) == dict
FROM: funcy           | listQ (<-is_list)     :: listQ(seq)  ; checks if seq is list
FROM: funcy           | tupleQ (<-is_tuple)   :: tupleQ(seq)  ; checks if seq is tuple
DEFN: fptk            | fnot                  :: fnot(f, *args, **kwargs) = not(f(*args, **kwargs))

=== Strings ===
DEFN: fptk            | strlen                :: strlen(text)  ; rename of len, underlines usage on strings
DEFN: fptk            | str_join              :: str_join(ss, sep='')  ; rearrangement of funcy.str_join, ss is seq of strings
DEFN: fptk            | str_replace           :: str_replace(string, old, new, count=-1)  ; str.replace method as a function
DEFN: fptk            | lowercase             :: lowercase(string)  ; str.lower method as a function
DEFN: fptk            | endswith              :: endswith(string, suffix) -> bool  ; str.endswith method as a function (but can't take start/end params)
DEFN: fptk            | strip                 :: strip(string, chars=None)  ; str.strip method as a function
DEFN: fptk            | lstrip                :: lstrip(string, chars=None)  ; str.lstrip method as a function
DEFN: fptk            | rstrip                :: rstrip(string, chars=None)  ; str.rstrip method as a function
DEFN: fptk            | enlengthen            :: enlengthen(string, target_len, char=' ', fill_tail=True)  ; adds char to string until target_len reached

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
DEFN: fptk            | read_file             :: read_file(file_name, encoding='utf-8') -> str  ; returns whole file content
DEFN: fptk            | write_file            :: write_file(text, file_name, mode='w', encoding='utf-8')  ; modes: 'w' - (over)write, 'a' - append, 'x' - exclusive creation

=== Benchmarking ===
DEFN: fptk            | with_execution_time   :: w_e_t(f, *, n=1, tUnit='ns', msg='') -> avrg_time_of_1_run_in_seconds, pretty_string, f_result  ; f_result is from 1st function execution
DEFN: fptk            | dt_print              :: dt_printer(* args, fresh_run=False)  ; starts timer on fresh run, prints time passed since previous call
```