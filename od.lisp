(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defparameter *synonyms* (make-instance 'synonym-table))

(defclass od ()
  ((dtype :accessor dtype :initarg :dtype))


(defclass od-direct (od)
  ((value :accessor value :initarg :value)))

(defclass od-indirect (od) ()  ;; dtype = char | number | string | pointer
  ((base :accessor base :initarg :base)
   (key :accessor key :initarg :key)))

(defclass od-initialized (od-indirect) ()
  ((value :accessor value :initarg :value)))

(defclass od-pointer (od-indirect) ())

;;;; toolbox: simplistic implementation of Operand Descriptors as sparse arrays - stacks of indexed values, no mutation, linear search from top
;;;;  of stack for first index that matches (multiple values with the same index can appear in the stack, but the most recent value with an
;;;;  index overrides all other values with that same index)

(defmethod value ((self symbol))
  ;; recursively unwind symbol to extract its underlying operand descriptor
  (let ((d (lookup *synonyms* self)))
    (value d)))

(defmethod value ((self od-indirect)) ;; char | number | string
  (vget (base self) (key self)))

(defmethod value ((self od-pointer))
  (let ((target (nth (key self) (base self))))
    (value target)))

(defmethod save ((self symbol) v)
  ;; recursively unwind symbol to extract its underlying operand descriptor
  (let ((d (lookup *synonyms* self)))
    (save d v)))

(defmethod save ((self od-direct) v)
  (setf (value self) v))

(defmethod save ((self od-indirect) v)
  (vput (base self) (key self) v))

(defmethod save ((self od-pointer) v)
  (let ((target (nth (key self) (base self))))
    (save target v)))


(defmethod function-name ((self od-direct))
  (assert (string= "function" (dtype self)))
  (first (value self)))
(defmethod function-inputs ((self od-direct))
  (assert (string= "function" (dtype self)))
  (second (value self)))
(defmethod function-outputs ((self od-direct))
  (assert (string= "function" (dtype self)))
  (third (value self)))
(defmethod return-type ((self od-direct))
  (let ((outs (function-outputs self)))
    (first outs)))
