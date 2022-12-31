(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; each allocation space is a sparse array - a stack that contains {key value} pairs
;; access of an item is a linear search for its key beginning at the top of the stack
;; "mutation" doesn't change anything, but simply pushes another pair onto the stack
;;   that contains the same key, overriding previous values at that key due to the
;;   linear search strategy (this can be later optimized to mutate values at given indices)


(defparameter *globals* (vstack))
(defparameter *code* (vstack))
(defparameter *constants* (vstack))
(defparameter temp (vstack))
(defparameter arg (vstack))
(defparameter parameter (vstack))
(defparameter result (vstack))

(defparameter *instructions* nil)
;; *synonyms* defined in od.lisp

(defparameter *scopes* (list temp arg parameter result))

(define-symbol-macro _ :_)

;; varod == variable-od
;; constod == constant od
;; pointerod == pointer-od
;; voidod = void od
;; funcod == function od
;; bifuncod = built-in function od

(defun varod (typename space offset)
  (make-instance 'od-indirect :dtype typename :base space :key offset))

(defun pointerod (space offset)
  (make-instance 'od-indirect :dtype "pointer" :base space :key offset))

(defun voidod ()
  (make-instance 'od-direct :dtype "void"))

(defun constod (typename value offset)
  (make-instance 'od-direct :dtype typename :base *constants* :key offset :value value))

(defun funcod (function-name inputs outputs)
  (make-instance 'od-direct :dtype "function" :base *code* :key function-name :value (list function-name inputs outputs)))


(defun bifuncod (function-name inputs outputs)
  (make-instance 'od-direct :dtype "built in function" :key function-name :value (list function-name inputs outputs)))

;; vm  

(defun $g-pushScope ()
  (loop for scope in *scopes*
        do (venter scope)))

(defun $g-popScope ()
  (loop for scope in *scopes*
        do (vexit scope)))

(defun $g-defsynonym (name od)
  (defsynonym *synonyms* name od))

(defun $ir-defsynonym (name od)
  (defsynonym *synonyms* name od))

(defun $ir-beginFunction (name)
  (let ((function-descriptor (lookup *synonyms* name)))
    ;; emit label and function prequel ...>> (name function-descriptor)
    ;; and, bind args to params, in correct order (a3 a2 a1) -> (p1 p2 p3)
    (let ((arg-pairs (reverse (top-scope-as-list arg))))
      (venter parameter)
      (bind-formals function-descriptor arg-pairs)
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
        (let ((opcode (first instruction))
              (operands (rest instruction)))
          (cond
             ((string-equal "$g-pushScope" opcode) (apply #'$g-pushScope operands))
             ((string-equal "$g-popScope" opcode) (apply #'$g-popScope operands))
             ((string-equal "$g-defsynonym" opcode) (apply #'$g-defsynonym operands))
             ((string-equal "$ir-defsynonym" opcode) (apply #'$ir-defsynonym operands))
             ((string-equal "$ir-beginFunction" opcode) (apply #'$ir-beginFunction operands))
             ((string-equal "$ir-endFunction" opcode) (apply #'$ir-endFunction operands))
             ((string-equal "$ir-return-from-function" opcode) (apply #'$ir-return-from-function operands))
             ((string-equal "$ir-call" opcode) (apply #'$ir-call operands))
             ((string-equal "$ir-save-return-value" opcode) (apply #'$ir-save-return-value operands))
             ((string-equal "$ir-freshargs" opcode) (apply #'$ir-freshargs operands))
             ((string-equal "$ir-pushArg" opcode) (apply #'$ir-pushArg operands))
             ((string-equal "$ir-disposeargs" opcode) (apply #'$ir-disposeargs operands))
             ((string-equal "$ir-createConstant" opcode) (apply #'$ir-createConstant operands))
             ((string-equal "$ir-freshreturns" opcode) (apply #'$ir-freshreturns operands))
             ((string-equal "$ir-disposereturns" opcode) (apply #'$ir-disposereturns operands))
             ((string-equal "$ir-createTemp" opcode) (apply #'$ir-createTemp operands))
             (t (assert nil)))
          ($-run))))))

(defmethod formals ((self od))
  (assert (eq 'function (dtype self)))
    (second self))

;;;

(defun reset-all ()
  (setf *globals* (vstack)
        *code* (vstack)
        *constants* (vstack)
        temp (vstack)
        arg (vstack)
        parameter (vstack)
        result (vstack)
        *instructions* (vstack)
        *synonyms* (make-instance 'synonym-table)))

(defun lookup-code (name)
  (vget *code* name))

(defun bind-formals (function-descriptor arg-pairs)
  (let ((fparams (formals function-descriptor)))
    (if (not (= (length fparams) (length arg-pairs)))
        (compiler-error (format nil "wrong number of arguments passed to function ~a (given ~a)" function-descriptor arg-pairs))
      (mapc #'(lambda (param-pair arg-pair)
                (let ((name (first param-pair))
                      (val  (second arg-pair)))
                  (vput parameter name val)))
            fparams arg-pairs))))

(defun compiler-error (message)
  (error message))

;;;
(defun irtest0 ()
  (reset-all)
  (let ((c (varod "char" temp 0)))
    (save c #\X)
    (format *standard-output* "~a~%" (value c))))

(defun irtest1 ()
  (reset-all)
  ;; create a character A in the globals area, at key 0
  (let ((c (varod "char" *globals* 0)))
    (save c #\A)
    (format *standard-output* "~a~%" (value c))
    ;; make a pointer to the character A, the pointer is in the globals area, too, at key 1
    (let ((p (pointerod *globals* 1)))
      (save p c)
      (format *standard-output* "*p=~a c=~a~%" (value p) (value c)))))


(defun irtest2 ()
  (reset-all)
  (let ((c (varod "char*" *globals* 0)))
    (save c "bcdef")
    (format *standard-output* "~a~%" (value c))
    ;; make a pointer to the character A, the pointer is in the globals area, too, at key 1
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
  (vput *code* "identity" *script-identity*)
  (vput *code* "main" *script-main*)
  (push *script-main* *instructions*)
  (vput arg "argc" 1)
  (vput arg "argv" "")
  ($-run))

(defun irtest ()
  (irtest1)
  (irtest2)
  (irtest3)
  (irtest4))

