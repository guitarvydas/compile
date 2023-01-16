(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defclass basic-operand-descriptor ()
  ((dtype :accessor dtype :initarg :dtype)))

(defclass operand-descriptor (basic-operand-descriptor)
  ((base :accessor base :initarg :base)
   (key :accessor key :initarg :key)))

(defclass literal-operand-descriptor (basic-operand-descriptor)
  ((value :accessor value :initarg :value)))

(defclass literal-index-operand-descriptor (basic-operand-descriptor)
  ((container :accessor container :initarg :container)
   (offset :accessor offset :initarg :offset :initform 0)))

(defclass literal-info-operand-descriptor (basic-operand-descriptor)
  ((info :accessor info :initarg :info)))

(defclass collection-operand-descriptor (basic-operand-descriptor)
  ;; for now, a Lisp list
  ((container :accessor container :initarg :container :initform nil)))

;;;; toolbox: simplistic implementation of Operand Descriptors as sparse arrays - stacks of indexed values, no mutation, linear search from top
;;;;  of stack for first index that matches (multiple values with the same index can appear in the stack, but the most recent value with an
;;;;  index overrides all other values with that same index)

(defmethod $get ((self string))
  ;; recursively unwind symbol to extract its underlying data descriptor
  (let ((d ($lookup *synonyms* self)))
    ($get d)))

(defun $coerce (ty v)
  (declare (ignore ty))
  v) ;; noop for this POC, might need to emit code in a real compiler

(defmethod $get ((self operand-descriptor))
  ($coerce (dtype self) (stget (base self) (key self))))

(defmethod $get ((self literal-operand-descriptor))
  (value self))

(defmethod $get ((self literal-index-operand-descriptor))
  (let ((collection-operand (container self)))
    (let ((lis (container collection-operand)))
      (nth (offset self) lis))))

(defmethod $get ((self literal-info-operand-descriptor))
  (info self))

(defmethod $get ((self collection-operand-descriptor))
  (error "internal error $get called for collection")) ;; this should probably return the container, but for now is an assert to catch bootstrap errors


(defmethod $save ((self operand-descriptor) v)  (stput (base self) (key self) v))
(defmethod $save ((self literal-operand-descriptor) v) (error "cannot assign to literal operand"))
(defmethod $save ((self literal-index-operand-descriptor) v) (error "cannot assign to literal index"))
(defmethod $save ((self literal-info-operand-descriptor) v) (error "cannot assign to literal info"))
(defmethod $save ((self collection-operand-descriptor) v)(error "cannot assign to collection operand descriptor"))


;; fdesc = ("name" inputs outputs)
;; where inputs is a alist and outputs is an alist
(defmethod function-name ((self literal-info-operand-descriptor))
  (assert (string= "function" (dtype self)))
  (first (info self)))
(defmethod function-inputs ((self literal-info-operand-descriptor))
  (assert (string= "function" (dtype self)))
  (second (info self)))
(defmethod function-outputs ((self literal-info-operand-descriptor))
  (assert (string= "function" (dtype self)))
  (third (info self)))
(defmethod return-type ((self literal-info-operand-descriptor))
  (assert (string= "function" (dtype self)))
  (let ((outs (function-outputs self)))
    (cadr outs)))
(defmethod formals ((self literal-info-operand-descriptor))
  (function-inputs self))
(defun formal-name (pair) (car pair))
(defun formal-type (pair) (cdr pair))
(defmethod builtinp ((self literal-info-operand-descriptor))
  (assert (string= "function" (dtype self)))
  (let ((signature (info self)))
    (and (<= (length signature) 4)
         (eq 'builtin (fourth signature)))))
(defmethod function-symbol ((self literal-info-operand-descriptor))
  (let ((fname (function-name self)))
    (if (stringp fname)
        (intern (string-upcase fname))
      (if (symbolp fname)
          fname
        (error (format nil "wanted function-symbol that is a string or a symbol, but got ~a for ~a"
                       (type-of fname) fname))))))


(defmethod $cpush ((od collection-operand-descriptor) v-od)
  (push v-od (container od)))

(defmethod $cappend ((od collection-operand-descriptor) v-od)
  (setf (container od)
	(append (container od) (list v-od))))



;; ------ repr -----

(defmethod print-object ((self operand-descriptor) stream)
  (format stream "~a.~a" (name (base self)) (key self)))

(defmethod print-object ((self literal-operand-descriptor) stream)
  (format stream "~a" (value self)))
(defmethod print-object ((self literal-info-operand-descriptor) stream)
  (format stream "~a(~a)>>(~a)" (function-name self) (formals self) (return-type self)))
(defmethod print-object ((self literal-index-operand-descriptor) stream)
  (format stream "[" )
  (print-object (container self) stream)
  (format stream "[~a]]" (offset self)))
(defmethod print-object ((self collection-operand-descriptor) stream)
  (format stream "~a" (container self)))
