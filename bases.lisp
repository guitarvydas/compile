(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; each allocation space is a sparse array - a stack that contains {key value} pairs
;; access of an item is a linear search for its key beginning at the top of the stack
;; "mutation" doesn't change anything, but simply pushes another pair onto the stack
;;   that contains the same key, overriding previous values at that key due to the
;;   linear search strategy (this can be later optimized to mutate values at given indices)


(defparameter *globals* 'undefined)
(defparameter *code* 'undefined)
(defparameter *synonyms* 'undefined)
(defparameter *globalConstants* 'undefined)
(defparameter *temps* 'undefined)
(defparameter *args* 'undefined)
(defparameter *parameters* 'undefined)
(defparameter *results* 'undefined)

(defun cold-start ()
  (setf *globals* (scoped-table "globals"))
  (setf *code* (scoped-table "code"))
  (setf *synonyms* (scoped-table "synonyms"))
  (setf *globalConstants* (scoped-table "globalConstants"))
  (setf *temps* (scoped-table "temps"))
  (setf *args* (scoped-table "args"))
  (setf *parameters* (scoped-table "parameters"))
  (setf *results* (scoped-table "results")))

(defparameter *scopes* nil)

(defun reset-all ()
  (setf *scopes* (list *globals* *code* *globalConstants* *synonyms* *temps* *args* *parameters* *results*))

  (dolist (space *scopes*)
    (reset space))
  )
