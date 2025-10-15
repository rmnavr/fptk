
    ; PYTHON

        ; in Python only one «*» is valid in func signature;
        ; so, those 2 are both valid:
        f(a, b, /, c, *args, **kwargs)
        f(a, b, /, c, *, d, **kwargs)

        f(a: int, b: int, /, c: int, *, d: int = 0, **kwargs: dict) -> None

        ; for args all args must have one type, «float» in this case
        f(*args: float, **kwargs: dict) -> None

    ; HY

        (f a b / c #* args #** kwargs)
        (f a b / c * d #** kwargs)

        (f #^ int a #^ int b / #^ int c #* #^ int [d 0] #** #^ dict kwargs)

