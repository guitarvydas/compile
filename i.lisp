(declaim (optimize (debug 3) (safety 3) (speed 0)))


(defparameter *instructions* nil)
;; *synonyms* defined in descriptor.lisp


(define-symbol-macro _ nil)


;; interpreter  

(defun $beginFunction (function-descriptor)
  ;; prologue code if any
  (declare (ignore function-descriptor)))

(defun $endFunction (function-descriptor)
  ;; epilogue code if any
  (declare (ignore function-descriptor)))

(defun $copy (v sugar dest-od)
  (declare (ignore sugar))
  ;; copy data from slot in operand1 into slot of operand2
    ($save dest-od v))

(defun $call (function-od)
  (let ((fname (function-name function-od)))
    (funcall (intern (string-upcase fname)))))

