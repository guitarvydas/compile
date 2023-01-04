(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; scoped table of values
;; {key, value} pairs are placed in the topmost scope 
;; typical lookup = find key searching downwards through all scopes, return first value found

(defclass scoped-table ()
  ((name :accessor name :initform nil :initarg :name)
   (stack :accessor stack :initform nil)
   (scope-stack :accessor scope-stack :initform nil)))

(defmethod stenter ((self scoped-table))
  (push (stack self) (scope-stack self)))

(defmethod stexit ((self scoped-table))
  (pop (scope-stack self))
  (setf (stack self) (scope-stack self)))
  

(defmethod stput ((self scoped-table) key v)
  (push (list key v) (stack self)))

(defmethod stpush ((self scoped-table) desc)
  (push (list "" desc) (stack self)))

(defmethod stget ((self scoped-table) key)
  ;; return first match in list of stacks, top-down search
  ;; the table might contains duplicate keys, but the most recent overrides all other duplicates
  ;; this could be implemented in many ways - I've chosen to use the stupidest, hopefully most clear way
  ;; basically, the strategy is to use a stack (alist, plist) of values for each variable, this used
  ;;  to be called "dynamic binding", before mutation corrupted the idea
  (labels ((top-down-search (stack-of-stacks key)
             (if (null stack-of-stacks)
                 (error (format nil "can't find /~a/ in ~a" key self))
               (let ((top-stack (first stack-of-stacks)))
                 (multiple-value-bind (match success)
                     (first-match key top-stack)
                   (if success
                       match
                     (top-down-search (rest stack-of-stacks) key))))))
           (first-match (key pairs)
             (if (null pairs)
                 (values nil nil)
               (if (equal key (first pairs))
                   (values (second pairs) t)
                 (first-match key (rest pairs))))))
    (top-down-search (stack self) key)))

(defmethod sttop-scope-as-list ((self scoped-table))
  (let ((result nil)
        (iterator (stack self))
        (next-scope (first (scope-stack self))))
    (loop while (not (eq iterator next-scope))
          do (progn
               (push (second (first iterator)) result)
               (setf iterator (rest iterator))))
    (reverse result)))


(defun scoped-table (name)
  (make-instance 'scoped-table :name name))

(defmethod repr ((self scoped-table))
  (format nil "~a: ~a" (name self) (stack self)))