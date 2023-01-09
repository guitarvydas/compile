(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defclass operand-descriptor ()
  ((dtype :accessor dtype :initarg :dtype)
   (indirection :accessor indirection :initarg :@ :initform 1)
   (base :accessor base :initarg :base)
   (key :accessor key :initarg :key)))

(defclass info-operand-descriptor (operand-descriptor)
  ((info :accessor info :initarg :info)))

(defclass literal-operand-descriptor (operand-descriptor)
  ((value :accessor value :initarg :value))
  (:default-initargs
   :@ 0))

;;;; toolbox: simplistic implementation of Data Descriptors as sparse arrays - stacks of indexed values, no mutation, linear search from top
;;;;  of stack for first index that matches (multiple values with the same index can appear in the stack, but the most recent value with an
;;;;  index overrides all other values with that same index)

(defmethod $get ((self string))
  ;; recursively unwind symbol to extract its underlying data descriptor
  (let ((d ($lookup *synonyms* self)))
    ($get d)))

(defmethod $get ((self operand-descriptor))
  (cond
   ((= 1 (indirection self))
    (coerce (dtype self) (stget (base self) (key self))))
   ((= 2 (indirection self))
    (let ((decremented-indirection (1- (indirection self))))
      (let ((pointer-desc (make-instance 'operand-descriptor 
					 :base (base self)
					 :dtype (dtype self)
					 :key (assert nil) ;???
					 :@ 1)))
        ($get pointer-desc))))
   ((> (indirection self) 2)
    (assert nil))
   (t (assert nil))))

(defmethod $get ((self literal-operand-descriptor))
  (value self))

(defmethod $save ((self string) v)
  ;; recursively unwind symbol to extract its underlying data descriptor
  (let ((d ($lookup *synonyms* self)))
    ($save d v)))

(defmethod $save ((self operand-descriptor) v)
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
