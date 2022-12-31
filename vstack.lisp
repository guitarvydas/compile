(declaim (optimize (debug 3) (safety 3) (speed 0)))

;;; stacks of keyed values 
;;;   pairs {key value}
(defclass vstack ()
  ((stack :accessor stack :initform nil)
   (scope-stack :accessor scope-stack :initform nil)))

(defmethod venter ((self vstack))
  (push (scope-stack self) (stack self)))

(defmethod vexit ((self vstack))
  (setf (stack self) (pop (scope-stack self))))
  

(defmethod vput ((self vstack) key v)
  (push (list key v) (stack self)))

(defmethod vget ((self vstack) key)
  (labels ((top-down-search (stack key)
             (if (null stack)
                 (assert nil)
               (let ((item (first stack)))
                 (if (equal (first item) key)
                     (second item)
                   (top-down-search (rest stack) key))))))
    (top-down-search (stack self) key)))

(defmethod top-scope-as-list ((self vstack))
  (let ((result nil)
        (iterator (stack self))
        (next-scope (first (scope-stack self))))
    (loop while (not (eq iterator next-scope))
          do (progn
               (push (first iterator) result)
               (setf iterator (rest iterator))))
    (reverse result)))


(defun vstack ()
  (make-instance 'vstack))
