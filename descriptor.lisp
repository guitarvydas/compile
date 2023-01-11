(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defclass basic-operand-descriptor ()
  ((dtype :accessor dtype :initarg :dtype)))

(defclass operand-descriptor (basic-operand-descriptor)
  ((indirection :accessor indirection :initarg :@ :initform 1)
   (base :accessor base :initarg :base)
   (key :accessor key :initarg :key)))

(defclass literal-operand-descriptor (basic-operand-descriptor)
  ((value :accessor value :initarg :value)))

(defclass literal-index-operand-descriptor (basic-operand-descriptor)
  ((target :accessor target :initarg :target)
   (offset :accessor offset :initarg :offset :initform 0)))
(defclass literal-stack-pointer-operand-descriptor (basic-operand-descriptor)
  ((target :accessor target :initarg :target)))

(defclass literal-info-operand-descriptor (basic-operand-descriptor)
  ((info :accessor info :initarg :info)))

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

(defmethod $get ((self literal-index-operand-descriptor))
  (let ((arr ($get (target self))))
    (nth (offset self) arr)))

(defmethod $get ((self literal-stack-pointer-operand-descriptor))
  (let ((arr ($get (target self))))
    (first arr)))

(defmethod $get ((self literal-operand-descriptor))
  (value self))

(defmethod $save ((self string) v)
  ;; recursively unwind symbol to extract its underlying data descriptor
  (let ((d ($lookup *synonyms* self)))
    ($save d v)))

(defmethod $save ((self operand-descriptor) v)
  (stput (base self) (key self) v))

(defmethod $save ((self literal-index-operand-descriptor) v)
  (stput (nth (offset self) (target self)) v))

(defmethod $save ((self literal-stack-pointer-operand-descriptor) v)
  (stput (first (target self)) v))


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
