(defun empty-space () (make-array 128))
(defparameter _ '_)

(defparameter *args* (empty-space))
(defparameter *params* (empty-space))
(defparameter *code* (empty-space))
(defparameter *byte-pointers* (empty-space))
(defparameter *bytes* (empty-space))
(defparameter *temp* (empty-space))
(defparameter *returns* (empty-space))

(defclass declarative-type ()
  ((typename :accessor typename :initarg :typename)
   (indirection :accessor indirection :initarg :indirection)
   (base :accessor base :initarg :base)
   (displacement :accessor displacement :initarg :displacement)
   (src :accessor src :initarg :src)))
(defclass declarative-type-args (declarative-type)
  ())
(defclass declarative-type-params (declarative-type)
  ())
(defclass declarative-type-code (declarative-type)
  ())
(defclass declarative-type-bytes (declarative-type)
  ())
(defclass declarative-type-temp (declarative-type)
  ())
(defclass declarative-type-returns (declarative-type)
  ())


(defun dty (ty k b d src)
  (case b
     (args (make-instance 'declarative-type-args :typename ty :indirection k :base *args* :displacement d :src src))
     (params (make-instance 'declarative-type-params :typename ty :indirection k :base *params* :displacement d :src src))
     (code (make-instance 'declarative-type-code :typename ty :indirection k :base *code* :displacement d :src src))
     (bytes (make-instance 'declarative-type-bytes :typename ty :indirection k :base *bytes* :displacement d :src src))
     (temp (make-instance 'declarative-type-temp :typename ty :indirection k :base *temp* :displacement d :src src))
     (returns (make-instance 'declarative-type-returns :typename ty :indirection k :base *returns* :displacement d :src src))
     (otherwise (assert nil))))

(defmethod fetch ((dty declarative-type))
  (if (= 1 (displacement dty))
      (aref (base dty) (displacement dty))
    (assert nil)))
   
(defmethod fetch ((dty-bytes declarative-type-bytes))
  (case (displacement dty-bytes)
    (1  (aref (base dty-bytes) (displacement dty-bytes)))
    (0 	(src dty-bytes))
    (2  (let ((index (aref *byte-pointers* (displacement dty-bytes))))
	  (aref *bytes* index)))
    (otherwise (assert nil))))

(defmethod reset ((dty declarative-type))
  (setf (base dty) (empty-space)))


;; for now, "assign" is only defined for the bytes space   
(defmethod assign ((dty declarative-type-bytes) v)
  (case (displacement dty-bytes)
    (1  (setf (aref (base dty-bytes) (displacement dty-bytes)) v))
    (0  ???)
    (2 
    (2  (let ((index (aref *byte-pointers* (displacement dty-bytes))))
	  (aref *bytes* index)))
    (otherwise (assert nil))))
  
