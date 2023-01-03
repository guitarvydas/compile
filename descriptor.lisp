(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defparameter *synonyms* (make-instance 'synonym-table))

(defclass operand-descriptor ()
  ((dtype :accessor dtype :initarg :dtype)))

(defclass initialized-operand-descriptor (operand-descriptor)
  ((dtype :accessor dtype :initarg :dtype)
   (value :accessor value :initarg :value :initform nil)))

(defclass info-operand-descriptor (operand-descriptor)
  ((dtype :accessor dtype :initarg :dtype)
   (info :accessor info :initarg :info :initform nil)))

(defclass pointer-operand-descriptor (operand-descriptor)
  ((dtype :accessor dtype :initarg :dtype)
   (od :accessor od :initarg :od :initform nil)))


(defclass data-descriptor ()
  ((dtype :accessor dtype :initarg :dtype)))

(defclass dd-direct (data-descriptor)
   ((value :accessor value :initarg :value :initform nil)))

(defclass dd-initialized-indirect (data-descriptor)
  ((base :accessor base :initarg :base)
   (key :accessor key :initarg :key)
   (value :accessor value :initarg :value)))


(defclass dd-indirect (data-descriptor)  ;; dtype = char | number | string | pointer
   ((base :accessor base :initarg :base)
    (key :accessor key :initarg :key)))

;;;; toolbox: simplistic implementation of Data Descriptors as sparse arrays - stacks of indexed values, no mutation, linear search from top
;;;;  of stack for first index that matches (multiple values with the same index can appear in the stack, but the most recent value with an
;;;;  index overrides all other values with that same index)

(defmethod value ((self symbol))
  ;; recursively unwind symbol to extract its underlying data descriptor
  (let ((d (lookup *synonyms* self)))
    (value d)))

(defmethod value ((self dd-indirect)) ;; char | number | string | addres
  (vget (base self) (key self)))

(defmethod save ((self symbol) v)
  ;; recursively unwind symbol to extract its underlying data descriptor
  (let ((d (lookup *synonyms* self)))
    (save d v)))

(defmethod save ((self dd-direct) v)
  )

(defmethod save ((self dd-indirect) v)
  (vput (base self) (key self) v))


(defmethod function-name ((self operand-descriptor))
  (assert (string= "function" (dtype self)))
  (first (value self)))
(defmethod function-inputs ((self operand-descriptor))
  (assert (string= "function" (dtype self)))
  (second (value self)))
(defmethod function-outputs ((self operand-descriptor))
  (assert (string= "function" (dtype self)))
  (third (value self)))
(defmethod return-type ((self operand-descriptor))
  (assert (string= "function" (dtype self)))
  (let ((outs (function-outputs self)))
    (first outs)))
(defmethod formals ((self operand-descriptor))
  (assert (string= "function" (dtype self)))
  (second self))

