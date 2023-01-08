(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; each allocation space is a sparse array - a stack that contains {key value} pairs
;; access of an item is a linear search for its key beginning at the top of the stack
;; "mutation" doesn't change anything, but simply pushes another pair onto the stack
;;   that contains the same key, overriding previous values at that key due to the
;;   linear search strategy (this can be later optimized to mutate values at given indices)


(defparameter *globals* (scoped-table "globals"))
(defparameter *code* (scoped-table "code"))
(defparameter *synonyms* (scoped-table "synonyms"))
(defparameter *globalConstants* (scoped-table "globalConstants"))
(defparameter *temps* (scoped-table "temps"))
(defparameter *args* (scoped-table "args"))
(defparameter *parameters* (scoped-table "parameters"))
(defparameter *results* (scoped-table "results"))

(defparameter *scopes* nil)

(defun reset-all ()
  (setf *instructions* nil)
  (setf *scopes* (list *globals* *code* *globalConstants* *synonyms* *temps* *args* *parameters* *results*))

  (dolist (space *scopes*)
    (reset space))
  )
