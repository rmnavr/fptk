
    (import fptk *) (require fptk *)

    (defclass [dataclass] Point []
        #^ float x #^ float y
        (defn getX [self] (return self.x)))

    ; get     -> lists, dict
    ; .       -> lists, dict, attrs/methods
    ; getattr -> attrs/methods

    ; - «get» is macro and connot be flipped
    ; - «->» combines bad with partial/flip

    (print (get [1 2 3] -1))
    (print (nth -1 [1 2 3]))
    (print (nth_ -1 [1 2 3]))

    (-> [1 2 3] (get 0) (print))
    (->> [1 2 3] (nth_ 1) (print))
    (-> [1 2 3] (flip nth_ 1) (print))

    (-> (Point 3 4) (getattr "x") (print))
    (->> (Point 3 4) (flip getattr "x") (print))
    (-> (Point 3 4) (getattrm .y) (print))

