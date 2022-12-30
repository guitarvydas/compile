(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defclass synonym-table ()
  ((table :accessor table :initform (make-hash-table :test 'equal))))

(defparameter *synonyms* (make-instance 'synonym-table))

(defmethod defsynonym ((self synonym-table) name od)
  ;; put the lval template od into a mapping table at "name"
  ;; for now, each entry is a stack, with the most recent entry being first and overriding
  ;;  all other entries in that slot
  (multiple-value-bind (stack success)
      (gethash name (table self))
    (declare (ignore stack))
     (if success
	 (push od (gethash name (table self)))
       (setf (gethash name (table self)) (list od)))))

(defmethod lookup ((self synonym-table) name)
  (gethash (table self) name))
