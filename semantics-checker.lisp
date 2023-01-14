(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defun $s-var (typename base key)
  (make-instance 'operand-descriptor :dtype typename :base base :key key))

(defun $s-literal (typename value)
  (make-instance 'literal-operand-descriptor :dtype typename :value value))

(defun $s-literal-index (c offset)
  (assert (eq 'Collection (type-of c)))
  (make-instance 'literal-index-operand-descriptor :container c :offset offset))

(defun $s-literal-stack-pointer (c)
  (assert (eq 'Collection (type-of c)))
  (make-instance 'literal-stack-pointer-operand-descriptor :container c))
