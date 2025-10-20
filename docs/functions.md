
---
fptk docs:
1. You are here -> [Functions and modules](https://github.com/rmnavr/fptk/blob/main/docs/functions.md)
2. [Basic macros](https://github.com/rmnavr/fptk/blob/main/docs/macros.md)
3. [Lens related macros](https://github.com/rmnavr/fptk/blob/main/docs/lens.md)
4. [Result type](https://github.com/rmnavr/fptk/blob/main/docs/resultM.md)
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
=== FP: Control flow ===
INFO: hy              | if /base/                :: (if check true false)
INFO: hy              | cond /base/              :: (cond check1 do1 ... true doT)
MACR: hyrule          | case
MACR: hyrule          | branch
MACR: hyrule          | unless
MACR: hyrule          | lif

=== FP: threading ===
MACR: fptk._macros    | fm                       :: (fm (* it 3))  ; anonymous function that accepts args in form of 'it' or '%1', '%2', ... '%9'
MACR: fptk._macros    | f>                       :: (f> (* %1 %2) 3 4)  ; calculate anonymous function (with fm-syntax)
INFO: py              | zip /base/               :: zip(*iterables) -> zip object
DEFN: fptk            | lzip                     :: lzip(*iterables) -> List  ; literally just list(zip(*iterables))
INFO: py              | map /base/               :: map(func, *iterables) -> map object
FROM: funcy           | lmap                     :: lmap(f, *seqs) -> List  ; list version of map
MACR: fptk._macros    | mapm                     ; same as map, but expects fm-syntax for func
MACR: fptk._macros    | lmapm                    ; same as lmap, but expects fm-syntax for func
FROM: itertools       | starmap                  :: starmap(function, iterable)
DEFN: fptk            | lstarmap                 :: lstarmap(function, iterable) -> list  ; list version of starmap
FROM: functools       | reduce                   :: reduce(function, sequence[, initial]) -> value  ; theory: reduce + monoid = binary-function for free becomes n-arg-function
FROM: funcy           | reductions               :: reductions(f, seq [, acc]) -> generator  ; returns sequence of intermetidate values of reduce(f, seq, acc)
FROM: funcy           | lreductions              :: lreductions(f, seq [, acc]) -> list  ; list version of reductions
FROM: funcy           | sums                     :: sums(seq [, acc]) -> generator  ; reductions with addition function
FROM: funcy           | lsums                    :: lsums(seq [, acc]) -> list  ; list version of sums
FROM: math            | product (<-prod)         :: product(iterable, /, *, start=1)  ; product([2, 3, 5]) = 30

=== FP: n-applicators ===
MACR: hyrule          | do_n                     :: (do_n   n #* body) -> None  ; expands to ~ (do body body body ...)
MACR: hyrule          | list_n                   :: (list_n n #* body) -> List
DEFN: fptk            | nested                   :: nested(n, f)  ; applicator f(...(f(***)))
DEFN: fptk            | apply_n                  :: apply_n(n, f, *args, **kwargs)  ; f(f(f(...f(*args, **kwargs))

=== APL: filtering ===
INFO: py              | filter /base/            :: filter(function or None, iterable) -> filter object  ; when f=None, checks if elems are True
FROM: funcy           | lfilter                  :: lfilter(pred, seq) -> List  ; funcy list version of extended filter
MACR: fptk._macros    | filterm                  :: (filterm f xs)  ; same as filter, but expects fm-syntax for func
MACR: fptk._macros    | lfilterm                 :: (lfilterm f xs)  ; list version of lfilterm
DEFN: fptk            | fltr1st                  :: fltr1st(f, seq) -> Optional elem  ; returns first found element (or None)
FROM: funcy           | reject (<-remove)        :: reject(pred, seq)-> iterator  ; same as filter, but checks for False
FROM: funcy           | lreject (<-lremove)      :: lreject(pred, seq) -> List  ; list version of reject
DEFN: fptk            | without                  :: without(items, seq) -> generator  ; subtracts items from seq (as a sets)
DEFN: fptk            | lwithout                 :: lwithout(items, seq) -> list  ; list version of reject
FROM: funcy           | takewhile                :: takewhile([pred, ] seq)  ; yields elems of seq as long as they pass pred
FROM: funcy           | dropwhile                :: dropwhile([pred, ] seq)  ; mirror of dropwhile
FROM: funcy           | filter_split (<-split)   :: filter_split(pred, seq) -> passed, rejected
FROM: funcy           | lfilter_split (<-lsplit) :: lfilter_split(pred,seq) -> passed, rejected  ; list version of filter_split
FROM: funcy           | bisect_at (<-split_at)   :: bisect_at(n, seq) -> start, tail  ; len of start will = n, works only with n>=0
DEFN: fptk            | lbisect_at               :: lbisect_at(n, seq) -> start, tail  ; list version of bisect_at, but also for n<0, abs(n) will be len of tail
FROM: funcy           | bisect_by (<-split_by)   :: bisect_by(pred, seq) -> taken, dropped  ; similar to (takewhile, dropwhile)
FROM: funcy           | lbisect_by (<-lsplit_by) :: lbisect_by(pred, seq) -> taken, dropped  ; list version of lbisect
FROM: itertools       | mask_sel (<-compress)    :: mask_sel('abc', [1,0,1]) -> iterator: 'a', 'c'
DEFN: fptk            | lmask_sel                :: lmask_sel(data, selectors) -> list
DEFN: fptk            | mask2idxs                :: mask2idxs(mask) -> list  ; mask is list like [1 0 1 0] or [True False True False], which will be converted to [0 2]
DEFN: fptk            | idxs2mask                :: idxs2mask(idxs) -> list  ; idxs is non-sorted list of integers like [0 3 2], which will be converted to [1 0 1 1]

=== APL: iterators and looping ===
FROM: itertools       | islice                   :: islice(iterable, stop), islice(iterable, start, stop[, step])  ; list(islice(inf_range(10), 2)) == [10, 11]
FROM: itertools       | inf_range (<-count)      :: inf_range(start [, step])  ; inf_range(10) -> generator: 10, 11, 12, ...
FROM: itertools       | cycle                    :: cycle(p)  ; cycle('AB') -> A B A B ...
FROM: itertools       | repeat                   :: repeat(elem [, n])  ; repeat(10,3) -> 10 10 10
DEFN: fptk            | lislice                  ; list version of islice: lislice
DEFN: fptk            | lcycle                   :: lcycle(p, n) -> list  ; takes first n elems from cycle(p)
DEFN: fptk            | lrepeat                  :: lrepeat(elem, n) -> list  ; unlike in repeat, n has to be provided
FROM: itertools       | concat (<-chain)         :: concat(*seqs) -> iterator
DEFN: fptk            | lconcat                  :: lconcat(*seqs) -> list  ; list(concat(*seqs))
FROM: funcy           | cat                      :: cat(seqs)  ; non-variadic version of concat
FROM: funcy           | lcat                     :: lcat(seqs)  ; non-variadic version of concat
FROM: funcy           | mapcat                   :: mapcat(f, *seqs)  ; maps, then concatenates
FROM: funcy           | lmapcat                  :: lmapcat(f, *seqs)  ; maps, then concatenates
FROM: funcy           | pairwise                 :: pairwise(seq) -> iterator  ; supposed to be used in loops, will produce no elems for seq with len <= 1
FROM: funcy           | with_prev                :: with_prev(seq, fill=None) -> iterator  ; supposed to be used in loops
FROM: funcy           | with_next                :: with_next(seq, fill=None) -> iterator  ; supposed to be used in loops

=== APL: working with lists ===
FROM: hyrule          | flatten                  ; flattens to the bottom, non-mutating
DEFN: fptk            | lprint                   :: lprint(seq, sep=None)  ; prints every elem of seq on new line
INFO: py              | reversed /base/          :: reversed(sequence) -> iterator
DEFN: fptk            | lreversed                :: lreversed(sequence)  ; list version of reversed
DEFN: fptk            | partition                :: partition(n, seq, *, step=None, tail=False) -> generator  ; splits seq to lists of len n, tail=True will allow including fewer than n items
DEFN: fptk            | lpartition               :: lpartition(n, seq, *, step=None, tail=False) -> List  ; simply list(partition(...))
FROM: funcy           | partition_by             :: partition_by(f, seq) -> iterator of iterators  ; splits when f(item) change
FROM: funcy           | lpartition_by            :: lpartition_by(f,seq) -> list of lists  ; list(partition_by(...))
DEFN: fptk            | lmulticut_by             :: lmulticut_by(pred, seq, keep_border=True, merge_border=False) -> list  ; cut at pred(elem)==True elems

=== APL: counting ===
DEFN: fptk            | count_occurrences        :: count_occurrences(elem, seq) -> int  ; rename of list.count method

=== Getters: idxs and keys ===
INFO: hy              | . /macro/                :: (. xs [n1] [n2] ...) -> xs[n1][n2]...  ; throws error when not found
INFO: hy              | get /macro/              :: (get xs n #* keys) -> xs[n][key1]...  ; throws error when not found
FROM: funcy           | nth                      :: nth(n, seq) -> Optional elem  ; 0-based index; works also with dicts
INFO: py              | slice /base/             :: (slice start end step)  ; returns empty list when not found
INFO: hy              | cut /macro/              :: (cut xs start end step) -> (get xs (slice start end step)) -> List  ; returns empty list when none found
FROM: hyrule          | assoc                    :: (assoc xs k1 v1 k2 v2 ...) ~ (setv (get xs k1) v1 (get xs k2) v2) -> None  ; also possible: (assoc xs :x 1)
MACR: hyrule          | ncut
FROM: funcy           | first                    :: first(seq) -> Optional elem
FROM: funcy           | second                   :: second(seq) -> Optional elem
DEFN: fptk            | third                    :: third(seq) -> Optional elem
DEFN: fptk            | fourth                   :: fourth(seq) -> Optional elem
DEFN: fptk            | beforelast               :: beforelast(seq) -> Optional elem
FROM: funcy           | last                     :: last(seq) -> Optional elem
DEFN: fptk            | rest                     :: rest(seq) -> List  ; drops 1st elem of list
DEFN: fptk            | butlast                  :: butlast(seq) -> List  ; drops last elem of list
DEFN: fptk            | drop                     :: drop(n, seq) -> List  ; drops n>=0 elems from start of the list; when n<0, drops from end of the list
DEFN: fptk            | take                     :: take(n, seq) -> List  ; takes n elems from start; when n<0, takes from end of the list
DEFN: fptk            | pick                     :: pick(ns, seq) -> List  ; throws error if some of ns doesn't exist; ns can be list of ints or dict keys

=== Getters: keys and attrs ===
INFO: py              | getattr /base/           :: getattr(object, name[, default]) -> value  ; arg name should be given as str
MACR: fptk._macros    | getattrm                 :: (getattrm Object 'attr') (getattrm Object .attr)  ; accepts fptk-style .attr syntax
FROM: funcy           | pluck                    :: pluck(key, mappings) -> generator  ; gets same key (or idx) from every mapping, mappings can be lists of lists/dicts/etc.
FROM: funcy           | lpluck                   :: lpluck(key, mappings) -> list
FROM: funcy           | pluck_attr               :: pluck_attr(attr, objects) -> generator  ; attr should be given as str
FROM: funcy           | lpluck_attr              :: lpluck_attr(attr, objects) -> list  ; list version of pluck_attr
MACR: fptk._macros    | pluckm                   :: (pluckm n xs) (pluckm key ys) (pluckm .attr zs)  ; accepts fptk-style .arg syntax
MACR: fptk._macros    | lpluckm                  ; list version of pluckm

=== Typing: Base ===
MACR: hyrule          | of                       ; (of List int) -> List[int]
MACR: fptk._macros    | f::                      ; (f:: int -> int -> (of Tuple int str))
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
FROM: typing          | TypeVar
FROM: typing          | Generic
FROM: funcy           | noneQ (<-isnone)
FROM: funcy           | notnoneQ (<-notnone)
DEFN: fptk            | oftypeQ                  :: (oftypeQ tp x) -> (= (type x) tp)
DEFN: fptk            | intQ                     :: intQ(x)  ; checks literally if type(x) == int, will also work with StrictInt from pydantic
DEFN: fptk            | floatQ                   :: floatQ(x)  ; checks literally if type(x) == float, will also work with StrictFloat from pydantic
DEFN: fptk            | numberQ                  :: numberQ(x)  ; checks for intQ or floatQ, will also work with StrictInt/StrictFloat from pydantic
DEFN: fptk            | strQ                     :: strQ(x)  ; checks literally if type(x) == str, will also work with StrictStr from pydantic
DEFN: fptk            | dictQ                    :: dictQ(x)  ; checks literally if type(x) == dict
FROM: funcy           | listQ (<-is_list)        :: listQ(value)  ; checks if value is list
FROM: funcy           | tupleQ (<-is_tuple)      :: tupleQ(value)  ; checks if value is tuple
FROM: funcy           | setQ (<-is_set)          :: setQ(value)  ; checks if value is set
FROM: funcy           | iteratorQ (<-is_iter)    :: iteratorQ(value)  ; checks if value is iterator
FROM: funcy           | iterableQ (<-iterable)   :: iterableQ(value)  ; checks if value is iterable

=== Typing: Strict ===
FROM: pydantic        | BaseModel
FROM: pydantic        | StrictInt                ; will be still of int type, but will perform strict typecheck when variable is created
FROM: pydantic        | StrictStr                ; will be still of str type, but will perform strict typecheck when variable is created
FROM: pydantic        | StrictFloat              ; will be still of float type, but will perform strict typecheck when variable is created
SETV: fptk            | StrictNumber             ; Union of StrictInt and StrictFloat
FROM: pydantic        | validate_call            ; decorator for type-checking func args
SETV: fptk            | validateF                ; same as validate_call but with option validate_return=True set (thus validating args and return type)

=== Math and logic: Basic math ===
FROM: hyrule          | inc                      :: inc(n)  ; = n + 1
FROM: hyrule          | dec                      :: dec(n)  ; = n - 1
FROM: hyrule          | sign                     :: sign(n)  ; will give 0 for n=0
FROM: operator        | neg                      :: neg(n)  ; = -1 * n
DEFN: fptk            | half                     :: half(x)  ; = x/2
DEFN: fptk            | double                   :: double(x)  ; = x*2
DEFN: fptk            | squared                  :: squared(x)  ; = pow(x,2)
DEFN: fptk            | reciprocal               :: reciprocal(x)  ; = 1/x ; throws error for x=0
FROM: math            | sqrt                     :: sqrt(n)  ; = √n
FROM: math            | dist                     :: dist(p, q) -> float  ; ≈ √((px-qx)² + (py-qy)² ...)
FROM: math            | hypot                    :: hypot(*coordinates)  ; = √(x² + y² + ...)
DEFN: fptk            | normalize                :: normalize(xs) -> xs  ; will throw error for zero-len vector
FROM: math            | exp                      :: exp(x)
FROM: math            | log                      :: log(x, base=math.e)
DEFN: fptk            | ln                       :: ln(x)  ; = math.log(x, math.e) ; coexists with log for clarity
FROM: math            | log10                    :: log10(x)
FROM: funcy           | evenQ (<-even)           :: evenQ(x)
FROM: funcy           | oddQ (<-odd)             :: oddQ(x)
DEFN: fptk            | zeroQ                    ; checks directly via (= x 0)
DEFN: fptk            | negativeQ                ; checks directly via (< x 0)
DEFN: fptk            | positiveQ                ; checks directly via (> x 0)

=== Math and logic: Trigonometry ===
FROM: math            | pi                       ; literally just float pi=3.14...
FROM: math            | sin                      :: sin(x)  ; x is in radians
FROM: math            | cos                      :: cos(x)  ; x is in radians
FROM: math            | tan                      :: tan(x)  ; x is in radians, will give smth like 1.6E+16 for x = pi
FROM: math            | degrees                  :: degrees(x)  ; x in radians is converted to degrees
FROM: math            | radians                  :: radians(x)  ; x in degrees is converted to radians
FROM: math            | acos                     :: acos(x)  ; x is in radians, result is between 0 and pi
FROM: math            | asin                     :: asin(x)  ; x is in radians, result is between -pi/2 and pi/2
FROM: math            | atan                     :: asin(x)  ; x is in radians, result is between -pi/2 and pi/2
FROM: math            | atan2                    :: atan2(y, x)  ; both signs are considered

=== Math and logic: Base operators to functions ===
FROM: operator        | and_                     ; 'and' as function
FROM: operator        | or_                      ; 'or' as function
FROM: operator        | not_                     ; 'not' as function
FROM: operator        | is_                      ; 'is' as function
FROM: operator        | xor
FROM: operator        | eq                       ; equal
FROM: operator        | neq (<-ne)               ; non-equal
FROM: operator        | gt                       ; greater than
FROM: operator        | lt                       ; less than
FROM: operator        | geq (<-ge)               ; greater or equal
FROM: operator        | leq (<-le)               ; less or equal
FROM: operator        | matmul                   ; '@' as function
FROM: operator        | div (<-truediv)          :: div(a, b)
DEFN: fptk            | minus                    :: minus(x, y) = x - y
DEFN: fptk            | dmul                     :: dmul(*args) = arg1 + arg2 + ...  ; 'dunder mul', '*' operator as a function
DEFN: fptk            | dadd                     :: dadd(*args) = arg1 + arg2 + ...  ; 'dunder add', '+' operator as a function
DEFN: fptk            | lmul                     :: lmul(*args) = arg1 * arg2 * ...  ; rename of * operator, underlines usage for list
DEFN: fptk            | smul                     :: smul(*args) = arg1 * arg2 * ...  ; rename of * operator, underlines usage for string
DEFN: fptk            | mul                      :: mul(*args)  ; multiplication as a monoid (will not give error when used with 0 or 1 args)
DEFN: fptk            | plus                     :: plus(*args)  ; addition as a monoid (will not give error when used with 0 or 1 args)
DEFN: fptk            | sconcat                  :: sconcat(*args)  ; string concantenation as a monoid (will not give error when used with 0 or 1 args)

=== Math and logic: Logic checks ===
DEFN: fptk            | fnot                     :: fnot(f, *args, **kwargs)  ; = not(f(*args, **kwargs))
DEFN: fptk            | eq_any                   :: eq_any(x, values)  ; = (or (eq x value1) (eq x value2) ...)
DEFN: fptk            | on                       :: on(f, check, x, y)  ; example: (on len eq xs ys) -> (eq (len xs) (len yx))
DEFN: fptk            | all_fs                   :: all_fs(fs, *args, **kwargs)  ; checks if all f(*args, **kwargs) are True
DEFN: fptk            | any_fs                   :: any_fs(fs, *args, **kwargs)  ; checks if any of f(*args, **kwargs) is True
DEFN: fptk            | trueQ                    :: trueQ(x)  ; checks directly via (= x True)
DEFN: fptk            | falseQ                   :: falseQ(x)  ; checks directly via (= x False)
DEFN: fptk            | oflenQ                   :: oflenQ(n, xs)  ; checks directly via (= (len xs) n)
DEFN: fptk            | zerolenQ                 :: zerolenQ(xs)  ; checks directly via (= (len xs) 0)

=== Math and logic: Random ===
FROM: random          | choice                   :: choice(seq) -> Elem  ; throws error for empty list
FROM: random          | randint                  :: randint(a, b) -> int  ; returns random integer in range [a, b] including both end points
FROM: random          | randfloat (<-uniform)    :: randfloat(a, b) -> float  ; range is [a, b) or [a, b] depending on rounding
FROM: random          | rand01 (<-random)        :: rand01() -> float  ; generates random number in interval [0, 1)

=== Strings: Basics ===
DEFN: fptk            | strlen                   :: strlen(text)  ; rename of len, underlines usage on strings
DEFN: fptk            | str_join                 :: str_join(ss, sep='')  ; rearrangement of funcy.str_join, ss is seq of strings
DEFN: fptk            | lowercase                :: lowercase(string)  ; str.lower method as a function
DEFN: fptk            | strip                    :: strip(string, chars=None)  ; str.strip method as a function
DEFN: fptk            | lstrip                   :: lstrip(string, chars=None)  ; str.lstrip method as a function
DEFN: fptk            | rstrip                   :: rstrip(string, chars=None)  ; str.rstrip method as a function
DEFN: fptk            | enlengthen               :: enlengthen(string, target_len, char=' ', on_tail=True)  ; adds char to string until target_len reached

=== Strings: Regex ===
FROM: re              | re_sub (<-sub)           :: re_sub(rpattern, replacement, string, count=0, flags=0)
FROM: re              | re_split (<-split)       :: re_split(rpattern, string)
FROM: funcy           | re_find                  :: re_find(rpattern, string, flags=0) -> str  ; returns first found
FROM: funcy           | re_test                  :: re_test(rpattern, string, ...) -> bool  ; tests if string has match (not neccessarily whole string)
FROM: funcy           | re_all                   :: re_all(rpattern, string, ...) -> List

=== IO ===
FROM: os.path         | file_existsQ (<-exists)  :: file_existsQ(filename)  ; also works on folders
FROM: os.path         | fileQ (<-isfile)         :: fileQ(filename)
FROM: os.path         | dirQ (<-isdir)           :: dirQ(filename)
DEFN: fptk            | read_file                :: read_file(file_name, encoding='utf-8') -> str  ; returns whole file content
DEFN: fptk            | write_to_file            :: write_file(text, file_name, mode='w', encoding='utf-8')  ; modes: 'w' - (over)write, 'a' - append, 'x' - exclusive creation

=== Lens ===
FROM: lenses          | lens                     ; main object of lenses library (for working with immutable structures)
MACR: fptk._macros    | lns                      ; macros for working with lens, see lens macros docs for details
MACR: fptk._macros    | &+                       ; macros for working with lens, see lens macros docs for details
MACR: fptk._macros    | &+>                      ; macros for working with lens, see lens macros docs for details
MACR: fptk._macros    | l>                       ; macros for working with lens, see lens macros docs for details
MACR: fptk._macros    | l>=                      ; macros for working with lens, see lens macros docs for details

=== Benchmarking ===
DEFN: fptk            | timing                   :: timing(f, *args, **kwargs) -> (float, Any)  ; returns tuple of execution time (in s) and result of f(*args, **kwargs)
DEFN: fptk            | dt_print                 :: dt_printer(* args, fresh_run=False)  ; starts timer on fresh run, prints time passed since previous call

=== Testing ===
MACR: fptk._macros    | assertm                  :: (assertm op arg1 arg2)  ; tests if (op arg1 arg2), for example (= 1 1)
MACR: fptk._macros    | gives_error_typeQ        ; example: (assertm gives_error_typeQ (get [1] 2) IndexError)

```