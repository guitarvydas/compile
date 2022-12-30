(defparameter *synonyms* nil)

(defclass od ()
  ((dtype :accessor dtype :initarg :dtype)
   (base :accessor base :initarg :base)
   (index :accessor index :initarg :index)))


(defclass od-direct (od)
  ((value :accessor value)))

(defclass od-indirect (od) ())  ;; dtype = char | number | string | pointer

(defclass od-pointer (od-indirect) ())

(defun lookup (name)
  (gethash name *synonyms*))

;;;; toolbox: simplistic implementation of Operand Descriptors as sparse arrays - stacks of indexed values, no mutation, linear search from top
;;;;  of stack for first index that matches (multiple values with the same index can appear in the stack, but the most recent value with an
;;;;  index overrides all other values with that same index)

(defmethod value ((self symbol))
  ;; recursively unwind symbol to extract its underlying operand descriptor
  (let ((d (lookup self)))
    (value d)))

(defmethod value ((self od-indirect)) ;; char | number | string
  (nth (index self) (base self)))

(defmethod value ((self od-pointer))
  (let ((target (nth (index self) (base self))))
    (value target)))

(defmethod save ((self symbol) v)
  ;; recursively unwind symbol to extract its underlying operand descriptor
  (let ((d (lookup self)))
    (save d v)))

(defmethod save ((self od-direct) v)
  (setf (value self) v))

(defmethod save ((self od-indirect) v)
  (setf (nth (index self) (base self)) v))

(defmethod save ((self od-pointer) v)
  (let ((target (nth (index self) (base self))))
    (save target v)))
