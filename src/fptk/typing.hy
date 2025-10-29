
    (export :objects [ dataclass
                       Enum
                       List Tuple TypedDict Dict Union
                       Generator Any Optional Callable Literal Type TypeVar Generic
                       noneQ notnoneQ oftypeQ intQ floatQ numberQ strQ dictQ
                       listQ tupleQ setQ iteratorQ iterableQ
                       ;; strict:
                       BaseModel StrictInt StrictStr StrictFloat StrictNumber
                       validate_call validateF
                     ]
            :macros  [ of
                       f::   ; fptk macro
                       def:: ; fptk macro
                     ])

; [GROUP] Typing: Base ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (require hyrule [of])         #_ "| example: (of List int) which is equiv to py-code: List[int]"
    (require fptk._macros [f::])  #_ "| example: (f:: int -> int => (of Tuple int str)) -> Callable[[int, int], Tuple[int,str]]"
    (require fptk._macros [def::])  #_ "| define function with signature; example: (def:: int -> int -> float fdivide [x y] (/ x y))"

    (import dataclasses [dataclass])
    (import enum        [Enum])
    (import typing      [List])
    (import typing      [Tuple])
    (import typing      [TypedDict])
    (import typing      [Dict])
    (import typing      [Union])
    (import typing      [Generator])
    (import typing      [Any])
    (import typing      [Optional])
    (import typing      [Callable])
    (import typing      [Literal])
    (import typing      [Type])
    (import typing      [TypeVar])
    (import typing      [Generic])

    ;; type checks:

    (import funcy [isnone  :as noneQ])
    (import funcy [notnone :as notnoneQ]) ;;

    #_ "oftypeQ(tp, x) | checks directly via (= (type x) tp)"
    (defn oftypeQ [tp x] "checks literally if type(x) == tp" (= (type x) tp))

    #_ "intQ(x) | checks literally if type(x) == int, will also work with StrictInt from pydantic"
    (defn intQ [x]
        "checks literally if type(x) == int"
        (= (type x) int))    

    #_ "floatQ(x) | checks literally if type(x) == float, will also work with StrictFloat from pydantic"
    (defn floatQ [x]
        "checks literally if type(x) == float"
        (= (type x) float))

    #_ "numberQ(x) | checks for intQ or floatQ, will also work with StrictInt/StrictFloat from pydantic"
    (defn numberQ [x]
        "checks literally if type(x) == int or type(x) == float"
        (= (type x) float))

    #_ "strQ(x) | checks literally if type(x) == str, will also work with StrictStr from pydantic"
    (defn strQ [x]
        "checks literally if type(x) == int or type(x) == float"
        (= (type x) str))

    #_ "dictQ(x) | checks literally if type(x) == dict"
    (defn dictQ [x]
        "checks literally if type(x) == dict"
        (= (type x) dict))

    (import funcy [is_list  :as listQ ])    #_ "listQ(value)     | checks if value is list"
    (import funcy [is_tuple :as tupleQ])    #_ "tupleQ(value)    | checks if value is tuple"
    (import funcy [is_set   :as setQ])      #_ "setQ(value)      | checks if value is set"
    (import funcy [is_iter  :as iteratorQ]) #_ "iteratorQ(value) | checks if value is iterator"
    (import funcy [iterable :as iterableQ]) #_ "iterableQ(value) | checks if value is iterable"

; _____________________________________________________________________________/ }}}1
; [GROUP] Typing: Strict ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1

    (import pydantic    [BaseModel])
    (import pydantic    [StrictInt])       #_ "will be still of int type, but will perform strict typecheck when variable is created"
    (import pydantic    [StrictStr])       #_ "will be still of str type, but will perform strict typecheck when variable is created"
    (import pydantic    [StrictFloat])     #_ "will be still of float type, but will perform strict typecheck when variable is created" ;;

    #_ "Union of StrictInt and StrictFloat"
    (setv StrictNumber (of Union #(StrictInt StrictFloat))) ;;

    (import pydantic [validate_call])   #_ "decorator for type-checking func args" ;;

    #_ "same as validate_call but with option validate_return=True set (thus validating args and return type)"
    (setv validateF (validate_call :validate_return True))

; _____________________________________________________________________________/ }}}1

