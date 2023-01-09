(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defmethod $defsynonym ((self scoped-table) name od)
  (stput self name od))
  
(defmethod $lookup ((self scoped-table) name)
  (stget self name))

(defun $pushNewScope (space)
  (stenter space)
    
(defun $popScope (space)
  (stexit space)
    
(defun $push (space synonym)
  (let ((desc ($get space synonym)))
    (stpush space desc)))
