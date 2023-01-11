(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defmethod $defsynonym ((self scoped-table) name od)
  (stput self name od))
  
(defmethod $lookup ((self scoped-table) name)
  (stget self name))

(defun $pushNewScope (base)
  (stenter base))
    
(defun $popScope (base)
  (stexit base))
    
(defun $push (base v)
  (stpush base v))
