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
  (setf (stack self) (first (scope-stack self)))
  (pop (scope-stack self)))
  

(defmethod stput ((self scoped-table) key v)
  (push (cons key v) (stack self)))

(defmethod stpush ((self scoped-table) v)
  (push (cons "" v) (stack self)))

(defmethod stget ((self scoped-table) key)
  ;; return first match in list of stacks, top-down search
  ;; the table might contains duplicate keys, but the most recent overrides all other duplicates
  ;; this could be implemented in many ways - I've chosen to use the stupidest, hopefully most clear way
  ;; basically, the strategy is to use a stack (alist, plist) of values for each variable, this used
  ;;  to be called "dynamic binding", before mutation corrupted the idea
  ;;
  ;; if "key" is a list of the form (@ NN), stget disregards the name and returns the NNth entry in the stack (0 => top
  ;;   of stack, 1 => 2nd item on the stack, etc.)
  (let ((value-pairs (stack self)))
    (labels ((first-match (key pairs)
               (cond
                ((null pairs)
                 (warning (format nil "can't find /~a/ in ~a" key self))
                 (values nil nil))
                (t (let ((keyval (first pairs)))
                     (let ((address (first keyval))
                           (data (cdr keyval)))
                       (cond
                        ((equal key address)
                         (values data t))
                        (t (first-match key (rest pairs))))))))))

	    (if (and (listp key)
		     (= 2 (length key))
		     (eq '@ (first key))
		     (numberp (second key)))
		(let ((pair (nth (second key) value-pairs)))
		  (cdr pair))
	      (first-match key value-pairs)))))

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

(defmethod keys-as-list ((self scoped-table))
  (let ((result nil))
    (mapcar #'(lambda (pair) (car pair)) (stack self))))

(defmethod as-list ((self scoped-table))
  (stack self))

(defun scoped-table (name)
  (make-instance 'scoped-table :name name))

(defmethod repr ((self scoped-table))
  (format nil "~a: ~a" (name self) (stack self)))


