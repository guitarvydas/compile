(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; each allocation space is a sparse array - a stack that contains {key value} pairs
;; access of an item is a linear search for its key beginning at the top of the stack
;; "mutation" doesn't change anything, but simply pushes another pair onto the stack
;;   that contains the same key, overriding previous values at that key due to the
;;   linear search strategy (this can be later optimized to mutate values at given indices)


(defparameter *globals* (vstack))
(defparameter *code* (vstack))
(defparameter *constants* (vstack))
(defparameter temp (vstack))
(defparameter arg (vstack))
(defparameter parameter (vstack))
(defparameter result (vstack))
