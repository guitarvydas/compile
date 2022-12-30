;; each allocation space is a sparse array - a stack that contains {index value} pairs
;; access of an item is a linear search for its index beginning at the top of the stack
;; "mutation" doesn't change anything, but simply pushes another pair onto the stack
;;   that contains the same index, overriding previous values at that index due to the
;;   linear search strategy (this can be later optimized to mutate values at given indices)


(defparameter *globals* (vstack))
(defparameter *code* (vstack))
(defparameter *constant-index* -1)
(defparameter *costants* (vstack))
(defparameter *scope* (vstack))
(defparameter temp (vstack))
(defparameter arg (vstack))
(defparameter parameter (vstack))
(defparameter result (vstack))

(defparameter *instructions* nil)
(defparameter *synonyms* nil)


(define-symbol-macro _ :_)

(defun next-constant ()
  (let ((n (incf *constant-index*)))
    n))

;; varod == variable-od
;; constod == constant od
;; pointerod == pointer-od
;; voidod = void od
;; funcod == function od
;; bifuncod = built-in function od

(defun varod (typename space offset)
  (make-instance 'od-indirect :dtype typename :base space :index offset))

(defun pointerod (space offset)
  (make-instance 'od-indirect :dtype "pointer" :base space :index offset))

(defun voidod (space offset)
  (make-instance 'od-direct :dtype "void"))

(defun constod (typename value)
  (make-instance 'od-direct :dtype typename :base *constants* :index (next-constant)
		 :value value))

(defun funcdod (function-name inputs outputs)
  (make-instance 'od-direct :dtype "function" :base *code* :index function-name :value (list function-name inputs outputs)))


(defun bifuncdod (function-name inputs outputs)
  (make-instance 'od-direct :dtype "built in function" :index function-name :value (list function-name inputs outputs)))

;; vm  

(defun $g-pushScope ()
  (push nil *scope*))

(defun $g-popScope ()
  (pop *scope*))

(defun defsynonym (name od)
  ;; put the lval template od into a mapping table at "name"
  ;; for now, each entry is a stack, with the most recent entry being first and overriding
  ;;  all other entries in that slot
  (multiple-value-bind (stack success) (gethash name *synonyms*) (declare (ignore stack))
     (if success
	 (push od (gethash name *synonyms*))
       (setf (gethash name *synonyms*) (list od)))))

(defun $g-defsynonym (name od)
  (defsynonym name od))

(defun $ir-defsynonym (name od)
  (defsynonym name od))

(defun lookup-synonym (name)
  (gethash name *synonyms*))

(defun $ir-beginFunction (name)
  (let ((function-descriptor (lookup-synonym name)))
    (declare (ignore name))
    ;; emit label and function prequel ...>> (name function-descriptor)

    ;; copy/move args to params, in correct order (a3 a2 a1) -> (p1 p2 p3)
    (let ((p nil))
      (loop for arg in *args*
	    do (push arg p))
      (venter *params*)
      (loop for param in p
	    do (push p *params*))

      ($-fresh-temps))))

(defun $ir-endFunction (name)
  (let ((function-descriptor (fetch name)))
    (declare (ignore function-descriptor))
    (vexit *parameters*)
    ($-dispose-temps)))

(defun $ir-return-from-function (od)
  (push (fetch od) *results*))

(defun $ir-call (od)
  (let ((script (fetch od)))
    (push *instructions* script)))

(defun $ir-save-return-value (function-descriptor od)
  (let ((rettype (return-type function-descriptor)))
    (save od (varod rettype *results*))))
  
(defun $ir-freshargs ()
  (venter *args*))
(defun $ir-pushArg (v)
  (push v *args*))
(defun $ir-disposeargs ()
  (vexit *args*))

(defun $ir-createConstant (name od)
  ;; in this version, createConstant is like defsynonym
  ;; in an optimizer, createConstant can poke a set of bytes into the *constants* space, and return an od to that place, which becomes a synonym
  ($g-defsynonym name od))

(defun $ir-freshreturns ()
  (venter *returns*))
(defun $ir-disposereturns ()
  (vexit *returns*))


(defun $ir-createTemp (d)
  (let ((od (value d)))

;; end vm



    


(defun $-fresh-temps ()
  (venter temp))
(defun $-dispose-temps ()
  (vexit temp))

(defun $-run ()
  (if (null *instructions*)
      nil
    (if (null (first *instructions*))
	(progn
	  (pop *instructions*)
	  ($-run))
      (let ((instruction (pop (first *instructions*))))
	(funcall instruction)))))

(defmethod formals ((self od))
  (assert (eq 'function (dtype self)))
  (let ((desc (description self)))
    (first desc)))

;;;

(defun reset-all ()
  (setf global (vstack)
        *code* (vstack)
        *costants* (vstack)
        *scope* (vstack)
        temp (vstack)
        arg (vstack)
        parameter (vstack)
        result (vstack)
        *instructions* nil
        *synonyms* nil)
  (setf *constant-index* -1))

  
;;;
(defun irtest0 ()
  (reset-all)
  (let ((c (od-char 1 *temps* 0)))
    (assign c #\X)
    (format *standard-output* "~a~%" (fetch c))))

(defun irtest1 ()
  (reset-all)
  ;; create a character A in the globals area, at index 0
  (let ((c (varod "char" *globals* 0)))
    (save c #\A)
    (format *standard-output* "~a~%" (fetch c))
    ;; make a pointer to the character A, the pointer is in the globals area, too, at index 1
    (let ((p (pointerod *globals* 1)))
      (save p c)
      (format *standard-output* "*p = ~a~%" (fetch p)))))


(defun irtest ()
  (irtest1))
