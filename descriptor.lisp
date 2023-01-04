(declaim (optimize (debug 3) (safety 3) (speed 0)))

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
  (stget (base self) (key self)))

(defmethod value ((self info-operand-descriptor))
 (info self))

(defmethod save ((self symbol) v)
  ;; recursively unwind symbol to extract its underlying data descriptor
  (let ((d (lookup *synonyms* self)))
    (save d v)))

(defmethod save ((self dd-direct) v)
  )

(defmethod save ((self dd-indirect) v)
  (stput (base self) (key self) v))


;; fdesc = ("name" inputs outputs)
;; where inputs is a alist and outputs is an alist
(defmethod function-name ((self info-operand-descriptor))
  (assert (string= "function" (dtype self)))
  (first (info self)))
(defmethod function-inputs ((self info-operand-descriptor))
  (assert (string= "function" (dtype self)))
  (second (info self)))
(defmethod function-outputs ((self info-operand-descriptor))
  (assert (string= "function" (dtype self)))
  (third (info self)))
(defmethod return-type ((self info-operand-descriptor))
  (assert (string= "function" (dtype self)))
  (let ((outs (function-outputs self)))
    (cadr outs)))
(defmethod formals ((self info-operand-descriptor))
  (function-inputs self))
(defun formal-name (pair) (car pair))
(defun formal-type (pair) (cdr pair))
(defmethod builtinp ((self info-operand-descriptor))
  (assert (string= "function" (dtype self)))
  (let ((signature (info self)))
    (and (<= (length signature) 4)
         (eq 'builtin (fourth signature)))))
(defmethod function-symbol ((self info-operand-descriptor))
  (let ((fname (function-name self)))
    (if (stringp fname)
        (intern (string-upcase fname))
      (if (symbolp fname)
          fname
        (error (format nil "wanted function-symbol that is a string or a symbol, but got ~a for ~a"
                       (type-of fname) fname))))))