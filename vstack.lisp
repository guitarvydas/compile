;;; stacks of indexed values 
;;;   pairs {index value}
(defclass sparse-array ()
  ((stack :accessor stack :initform nil)))

(defmethod vput ((self sparse-array) index v)
  (push (list index v) (stack self)))

(defmethod vget ((self sparse-array) index)
  (labels ((top-down-search (stack index)
             (if (null stack)
                 (assert nil)
               (let ((item (first stack)))
                 (if (eq (first item) index)
                     (second item)
                   (top-down-search (rest stack) index))))))
    (top-down-search (stack self) index)))
                      
(defun vstack ()
  (make-instance 'sparse-array))
