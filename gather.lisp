(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defun $g-funcod (function-name inputs outputs)
  (make-instance 'od-direct :dtype "function" :base *code* :key function-name :value (list function-name inputs outputs)))


(defun $g-bifuncod (function-name inputs outputs)
  (make-instance 'od-direct :dtype "built in function" :key function-name :value (list function-name inputs outputs)))
