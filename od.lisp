(defclass od ()
  ((type :accessor type :initarg :type)
   (indirection :accessor indirection :initarg :indirection)
   (base :accessor base :initarg :base)
   (index :accessor index :initarg :index)))

(defun od-char (k b i)
  (make-instance 'od :type 'char :base b :index (-1 i)))

(defun od-function (k b i)
  (make-instance 'od :type 'function :base b :index i)

(defun od-void (k b i)
  (make-instance 'od :type 'void :base b :index i))

(defmethod od-fetch ((self od-char))
  )
(defmethod od-fetch ((self od-function))
  )
(defmethod od-fetch ((self od-void))
  'void)



(defun fetch (x)
  (if (symbolp x)
      ()
    (od-fetch x)))