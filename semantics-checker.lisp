(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defun $s-var (typename space key)
  (make-instance 'operand-descriptor :dtype typename :base space :key offset))

(defun $s-literal (typename space key value)
  (make-instance 'literal-operand-descriptor :dtype typename :base space :key offset :value value))
