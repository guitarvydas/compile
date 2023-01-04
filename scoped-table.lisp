(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; scoped table of values
;; {key, value} pairs are placed in the topmost scope 
;; typical lookup = find key searching downwards through all scopes, return first value found

(defclass scoped-table ()
  ((name :accessor name :initform nil :initarg :name)
   (stack :accessor stack :initform nil)
   (scope-stack :accessor scope-stack :initform nil)))

(defmethod reset ((self scoped-table))
  (setf (stack self) nil)
  (setf (scope-stack self) nil))

(defmethod stenter ((self scoped-table))
  (push (stack self) (scope-stack self)))

(defmethod stexit ((self scoped-table))
  (pop (scope-stack self))
  (setf (stack self) (scope-stack self)))
  

(defmethod stput ((self scoped-table) key v)
  (push (cons key v) (stack self)))

(defmethod stpush ((self scoped-table) desc)
  (push (cons "" desc) (stack self)))

(defmethod stget ((self scoped-table) key)
  ;; return first match in list of stacks, top-down search
  ;; the table might contains duplicate keys, but the most recent overrides all other duplicates
  ;; this could be implemented in many ways - I've chosen to use the stupidest, hopefully most clear way
  ;; basically, the strategy is to use a stack (alist, plist) of values for each variable, this used
  ;;  to be called "dynamic binding", before mutation corrupted the idea
  (let ((value-pairs (stack self)))
    (labels ((first-match (key pairs)
               (if (null pairs)
                   (progn
                     (warning (format nil "can't find /~a/ in ~a" key self))
                     (values nil nil))  
                 (let ((keyval (first pairs)))
                   (if (equal key (car keyval))
                       (values (cdr keyval) t)
                     (first-match key (rest pairs)))))))
      (first-match key value-pairs))))

(defun warning (message)
  (error message)) ;; maybe, after debugging, we will allow non-matches

(defmethod sttop-scope-as-list ((self scoped-table))
  (let ((result nil)
        (iterator (stack self))
        (next-scope (first (scope-stack self))))
    (loop while (not (eq iterator next-scope))
          do (progn
               (push (cdr (first iterator)) result)
               (setf iterator (rest iterator))))
    (reverse result)))


(defun scoped-table (name)
  (make-instance 'scoped-table :name name))

(defmethod repr ((self scoped-table))
  (format nil "~a: ~a" (name self) (stack self)))