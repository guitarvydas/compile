(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; each allocation space is a sparse array - a stack that contains {key value} pairs
;; access of an item is a linear search for its key beginning at the top of the stack
;; "mutation" doesn't change anything, but simply pushes another pair onto the stack
;;   that contains the same key, overriding previous values at that key due to the
;;   linear search strategy (this can be later optimized to mutate values at given indices)


(defparameter *globals* nil)
(defparameter *code* nil)
(defparameter *synonyms* nil)
(defparameter *constants* nil)
(defparameter *temp* nil)
(defparameter *arg* nil)
(defparameter *parameter* nil)
(defparameter *result* nil)

(defparameter *scopes* nil)

(defun reset-all ()
  (setf *globals* (scoped-table "globals")
        *code* (scoped-table "code")
        *constants* (scoped-table "constants")
        *temp* (scoped-table "temp")
        *arg* (scoped-table "arg")
        *parameter* (scoped-table "parameter")
        *result* (scoped-table "result")
        *synonyms* (scoped-table "synonyms")

        *instructions* nil
        *scopes* (list *globals* *code* *constants* *synonyms* *temp* *arg* *parameter* *result*)
))
