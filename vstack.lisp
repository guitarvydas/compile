(declaim (optimize (debug 3) (safety 3) (speed 0)))

;;; stacks of indexed values 
;;;   pairs {index value}
(defclass vstack ()
  ((stack :accessor stack :initform nil)
   (scope-stack :accessor scope-stack :initform nil)))

(defmethod venter ((self vstack))
  (push (scope-stack self) (stack self)))

(defmethod vexit ((self vstack))
  (setf (stack self) (pop (scope-stack self))))
  

(defmethod vput ((self vstack) index v)
  (push (list index v) (stack self)))

(defmethod vget ((self vstack) index)
  (labels ((top-down-search (stack index)
             (if (null stack)
                 (assert nil)
               (let ((item (first stack)))
                 (if (eq (first item) index)
                     (second item)
                   (top-down-search (rest stack) index))))))
    (top-down-search (stack self) index)))
                      
(defun vstack ()
  (make-instance 'vstack))
