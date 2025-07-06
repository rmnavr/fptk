
<!-- Intro ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# FPTK Doc Generator

Purpose of **FPTK Doc Generator** is to auto-generate short doc table of all
fptk imports/funcs/classes with short description.

```hy
=== Group name 1 ===
TYPE source_lib | entity_name   :: signature ; description
TYPE source_lib | entity_name   :: signature ; description

=== Group name 2 ===
...
```

Simply run `doc_generator.hy` file to generate this table.

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Recognition: Grouping ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

# Recognized entities

For generator to work, two main fptk files (`fpext.hy` and `macro.hy`) need to use special syntax.

Below, "signature" and "description" are arbitrary texts. No special rules are enforced.

## Grouping

Only code incide Vim Cells (of Lvl 1) marked with `[GROUP]` will be recognized:
```hy
; [GROUP] Group name ‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {({1

    ...
    (import math) #_ "some comment"
    ...

; _________________________________/ })}1
```

Inside groups, line-comments MUST use two semicolons:
```hy
    ;  ✗ when inside Vim Cell, this comment will break code recognition

    ;; ✓ such a comment is OK
```

If entities defined via `(setv ...)` or `(defn ...)` follow entities defined via `(import ...)`
it is strongly recommended to separate them with `;;` comment.
Reason for this is because parser might be confused with to what entity to attach `#_ ...` :
```hy
    (import math) #_ "description"
    (defn some_func [x y] ...)
```

<!-- __________________________________________________________________________/ }}}1 -->
<!-- Recognition: Entities ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\ {{{1 -->

## Entities

General rules:
1. For most entities `|` acts as a smart separator in comments (so you can't use it in the comment text itself)
2. Only one entity is allowed per import (code like `(import math [log exp])` will not be recognized)
3. Comment `#_ "..."` is optional. You can drop entire `#_ "..."` expression.

Entities that support optional description:
```hy
    ;; import whole module:
    (import math) #_ "optional description"

    ;; define fptk variable:
    #_ "optional description"
    (setv xsmth (+ 1 2))
```

Entities that support optional signature and description:
```hy
    ;; import smth from module:
    (import funcy [partial])       #_ "optional signature and description"
    (import operator [ne :as neq]) #_ "optional signature and description"

    ;; require macro from module:
    (require hyrule [of]) #_ "optional signature and description"

    ;; define fptk function:
    #_ "optional signature and description"
    (defn [optional_decorator] some_func [x y] ...)

    ;; ==========================================

    ;; possible comment format:
    #_ "signature |"
    #_ "description"
    #_ "signature | description"
```

Dummy functions (intention for adding this to full list of funcs is for getting whole picture of all py+hy+fptk functions):
```hy
    (comment "source_lib | kind_str | name | signature | description ")

    ;; example:
    (comment "hy | func | get | (get xs n) | basic getter")

    ;; signature and description fields can be ommited, just keep "|" at place:
    (comment "hy | func | get ||")
```

<!-- __________________________________________________________________________/ }}}1 -->

