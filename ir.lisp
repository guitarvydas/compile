(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; each allocation space is a sparse array - a stack that contains {index value} pairs
;; access of an item is a linear search for its index beginning at the top of the stack
;; "mutation" doesn't change anything, but simply pushes another pair onto the stack
;;   that contains the same index, overriding previous values at that index due to the
;;   linear search strategy (this can be later optimized to mutate values at given indices)


(defparameter *globals* (vstack))
(defparameter *code* (vstack))
(defparameter *constant-index* -1)
(defparameter *constants* (vstack))
(defparameter *scope* (vstack))
(defparameter temp (vstack))
(defparameter arg (vstack))
(defparameter parameter (vstack))
(defparameter result (vstack))

(defparameter *instructions* nil)
;; *synonyms* defined in od.lisp

(define-symbol-macro _ :_)

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

(defun voidod ()
  (make-instance 'od-direct :dtype "void"))

(defun constod (typename value offset)
  (make-instance 'od-direct :dtype typename :base *constants* :index offset :value value))

(defun funcod (function-name inputs outputs)
  (make-instance 'od-direct :dtype "function" :base *code* :index function-name :value (list function-name inputs outputs)))


(defun bifuncod (function-name inputs outputs)
  (make-instance 'od-direct :dtype "built in function" :index function-name :value (list function-name inputs outputs)))

;; vm  

(defun $g-pushScope ()
  (push nil *scope*))

(defun $g-popScope ()
  (pop *scope*))

(defun $g-defsynonym (name od)
  (defsynonym *synonyms* name od))

(defun $ir-defsynonym (name od)
  (defsynonym *synonyms* name od))

(defun $ir-beginFunction (name)
  (let ((function-descriptor (lookup *synonyms* name)))
    (declare (ignore name function-descriptor))
    ;; emit label and function prequel ...>> (name function-descriptor)

    ;; copy/move args to params, in correct order (a3 a2 a1) -> (p1 p2 p3)
    (let ((p nil))
      (loop for a in arg
	    do (push a p))
      (venter parameter)
      (loop for a in p
	    do (push a parameter))

      ($-fresh-temps))))

(defun $ir-endFunction (name)
  (let ((function-descriptor (value name)))
    (declare (ignore function-descriptor))
    (vexit parameter)
    ($-dispose-temps)))

(defun $ir-return-from-function (od)
  (push (value od) result))

(defun $ir-call (od)
  (let ((function-descriptor (value od)))
    (let ((script (lookup-code (function-name function-descriptor))))
      (push *instructions* script))))

(defun $ir-save-return-value (function-descriptor od)
  (let ((rettype (return-type function-descriptor)))
    (save od (varod rettype result 0))))
  
(defun $ir-freshargs ()
  (venter arg))
(defun $ir-pushArg (v)
  (push v arg))
(defun $ir-disposeargs ()
  (vexit arg))

(defun $ir-createConstant (name od)
  ;; in this version, createConstant is like defsynonym
  ;; in an optimizer, createConstant can poke a set of bytes into the *constants* space, and return an od to that place, which becomes a synonym
  ($g-defsynonym name od))

(defun $ir-freshreturns ()
  (venter result))
(defun $ir-disposereturns ()
  (vexit result))


(defun $ir-createTemp (d)
  (let ((od (value d)))
    ;; no-op in this (non-optimized) version
    (declare (ignore od))))

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
        (let ((opcode (string-downcase (first instruction)))
              (operands (rest instruction)))
            (cond
             ((string= "$g-pushScope" opcode) (apply #'$g-pushScope operands))
             ((string= "$g-popScope" opcode) (apply #'$g-popScope operands))
             ((string= "$g-defsynonym" opcode) (apply #'$g-defsynonym operands))
             ((string= "$ir-defsynonym" opcode) (apply #'$ir-defsynonym operands))
             ((string= "$ir-beginFunction" opcode) (apply #'$ir-beginFunction operands))
             ((string= "$ir-endFunction" opcode) (apply #'$ir-endFunction operands))
             ((string= "$ir-return-from-function" opcode) (apply #'$ir-return-from-function operands))
             ((string= "$ir-call" opcode) (apply #'$ir-call operands))
             ((string= "$ir-save-return-value" opcode) (apply #'$ir-save-return-value operands))
             ((string= "$ir-freshargs" opcode) (apply #'$ir-freshargs operands))
             ((string= "$ir-pushArg" opcode) (apply #'$ir-pushArg operands))
             ((string= "$ir-disposeargs" opcode) (apply #'$ir-disposeargs operands))
             ((string= "$ir-createConstant" opcode) (apply #'$ir-createConstant operands))
             ((string= "$ir-freshreturns" opcode) (apply #'$ir-freshreturns operands))
             ((string= "$ir-disposereturns" opcode) (apply #'$ir-disposereturns operands))
             ((string= "$ir-createTemp" opcode) (apply #'$ir-createTemp operands))
             (t (assert nil))))))))

(defmethod formals ((self od))
  (assert (eq 'function (dtype self)))
    (second self))

;;;

(defun reset-all ()
  (setf *globals* (vstack)
        *code* (vstack)
        *constants* (vstack)
        *scope* (vstack)
        temp (vstack)
        arg (vstack)
        parameter (vstack)
        result (vstack)
        *instructions* (vstack)
        *synonyms* (make-instance 'synonym-table)))

  
;;;
(defun irtest0 ()
  (reset-all)
  (let ((c (varod "char" temp 0)))
    (save c #\X)
    (format *standard-output* "~a~%" (value c))))

(defun irtest1 ()
  (reset-all)
  ;; create a character A in the globals area, at index 0
  (let ((c (varod "char" *globals* 0)))
    (save c #\A)
    (format *standard-output* "~a~%" (value c))
    ;; make a pointer to the character A, the pointer is in the globals area, too, at index 1
    (let ((p (pointerod *globals* 1)))
      (save p c)
      (format *standard-output* "*p=~a c=~a~%" (value p) (value c)))))


(defun irtest2 ()
  (reset-all)
  (let ((c (varod "char*" *globals* 0)))
    (save c "bcdef")
    (format *standard-output* "~a~%" (value c))
    ;; make a pointer to the character A, the pointer is in the globals area, too, at index 1
    (let ((p (pointerod *globals* 1)))
      (save p c)
      (format *standard-output* "*p=~a c=~a~%" (value p) (value c)))))

(defun irtest3 ()
  (reset-all)
  (let ((fidentity (funcod
                    "identity"
                    (list "char") ;; param - c
                    (list "char")))) ;; return type - char
    (format *standard-output* "~a~%" (value fidentity))))

(defun irtest4 ()
  (reset-all)
  (vput *code* 0 *script-identity*)
  (vput *code* 1 *script-main*)
  (push *script-main* *instructions*)
  ($-run))

(defun irtest ()
  (irtest4))

