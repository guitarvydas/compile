(defclass od ()
  ((dtype :accessor dtype :initarg :dtype)
   (indirection :accessor indirection :initarg :indirection)
   (base :accessor base :initarg :base)
   (index :accessor index :initarg :index)
   (description :accessor description :initform nil :initarg :description)))


;;;; toolbox: simplistic implementation of Operand Descriptors as sparse arrays - stacks of indexed values, no mutation, linear search from top
;;;;  of stack for first index that matches (multiple values with the same index can appear in the stack, but the most recent value with an
;;;;  index overrides all other values with that same index)

(defmethod od-fetch ((self od))
  (case (indirection self)
    (0 (index self))
    (1 (let ((v (vget (base self) (index self)))
             (dty (dtype self)))
         (case dty
           (pointer (assert (numberp v)))
           (number (assert (numberp v)))
           (char (if (characterp v)
                     nil
                   (if (numberp v)
                       (setf v (code-char v))
                     (if (stringp v)
                         (setf v (char v 0))
                       (assert nil)))))
           (void (setf v 'void))
           (function (assert (symbolp v)))
           (otherwise (assert nil)))
         v))
    (otherwise
     (let ((pointer (fetch (od 'pointer 1 (base self)  (index self)))))
       (fetch (od (dtype self) (1- (indirection self)) (base self) pointer))))))

(defun fetch (x)
  (if (symbolp x)
      ()
    (od-fetch x)))

(defmethod assign ((self od) v)
  (assert (not (equal (dtype self) 'void)))
  (case (indirection self)
    (0 (setf (index self) v))
    (1 (vput (base self) (index self) v))
    (otherwise
     (let ((pointer (fetch (od 'pointer 1 (base self) (index self)))))
       (let ((obj (od (dtype self) (1- (indirection self)) (base self) pointer)))
         (assign obj v))))))

(defun od (dtype k b i &optional (description nil))
  (make-instance 'od :dtype dtype :indirection k :base b :index (1- i) :description description))


