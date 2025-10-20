
---
fptk docs:
1. [Functions and modules](https://github.com/rmnavr/fptk/blob/main/docs/functions.md)
2. [Basic macros](https://github.com/rmnavr/fptk/blob/main/docs/macros.md)
3. [Lens related macros](https://github.com/rmnavr/fptk/blob/main/docs/lens.md)
4. You are here -> [Result type](https://github.com/rmnavr/fptk/blob/main/docs/resultM.md)
---

# Intro to Result type in fptk

> Usage of Result type (or Either monad) pattern is best described in:
> [F#-lang: Railway Oriented Programming](https://fsharpforfunandprofit.com/rop/)

Terminology used in fptk implementation of Result type:
- Result — main type
- Container — Success or Failure type
- Value — value wrapped in Success or Failure type

Result in fptk aims to be compatible with pydantic typecheck.

> **Dev note**:
> fptk implementation of Result type contains some tricks
> (required for proper pydantic checks) that are hidden from user API.
> 
> Most notable thing is that `Success` and `Failure` are
> implemented as factory functions, not as classes (despite the fact that they start with a capital letter).
> And actual types are called `_Success` and `_Failure` (which are not exposed to user).

# User API

Creating Result type:
```hy
    (Success 3)      ; Create Result type with Success container (with 3 placed inside it)
    (Failure "err")  ; Create Result type with Failure container (with "err" placed inside it)

    ; From a user POV Result type itself must be used only in annotations,
    ; that can be optionally validated by pydantic's validate_call like for example:
    (defn [validate_call] f [#^ (of Result int str) x] (print x))

    ; output can be validated by pydantic too:
    (defn [(validate_call :validate_return True)]
          #^ (of Result int str) f [#^ int x] (Success x))
```

Utilities:
```hy
    (successQ r) -> bool  ; Checks if Result contains Success
    (failureQ r) -> bool  ; Checks if Result contains Failure

    (mapR r pureF1 pureF2 ...)  ; Apply pure functions to value stored in Success
                                ; or do nothing for Failure;
                                ; It is user's responsibility to ensure pureFi are indeed pure

    (bindR r monadicF1 monadicF2 ...)   ; Apply functions of signagure [f :: val -> Result]
                                        ; to value stored in Success or do nothing for Failure

    (unwrapR    r)          ; returns contained Success value or throws error when not Success
    (unwrapR_or r default)  ; returns contained Success value or falls back to default
    (unwrapE    r)          ; returns contained Failure value or throws error when not Failure
    (unwrapE_or r default)  ; returns contained Failure value or falls back to default

    ; If you need to access contained value no matter if it is Success or Failure, use:
    r.value


```

Dev usage:
```hy
    ; If you really need to access container _Success (or _Failure) 
    ; (although API is intentionally built to avoid such a usecase), use:
    r.result
```

