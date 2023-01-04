(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defun $g-func (function-name inputs outputs)
  (make-instance 'info-operand-descriptor :dtype "function" :info (list function-name inputs outputs)))


(defun $g-bifunc (function-name inputs outputs)
  (let ((fdesc ($g-func function-name inputs outputs)))
    (setf (info fdesc)
          (append (info fdesc) '(builtin)))
    fdesc))


(defun $g-void ()
  (make-instance 'info-operand-descriptor :dtype "void" :info nil))
