(declaim (optimize (debug 3) (safety 3) (speed 0)))


(defparameter *instructions* nil)
;; *synonyms* defined in descriptor.lisp

(defparameter *scopes* (list temp arg parameter result))

(define-symbol-macro _ :_)

(defun $ir-var (typename space offset)
  ($a-var typename space offset))


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

(defun $ir-call (name)
  (let ((function-descriptor (lookup-synonym name)))
    (let ((script (lookup-code (function-name function-descriptor))))
      (push *instructions* script))))

(defun $ir-save-return-value (function-descriptor od)
  (let ((rettype (return-type function-descriptor)))
    (save od ($ir-var rettype result 0))))
  
(defun $ir-freshargs ()
  (venter arg))
(defun $ir-pushArg (v)
  (let ((d (lookup *synonyms* v)))
    (vpush arg d)))
(defun $ir-disposeargs ()
  (vexit arg))

(defun $ir-initialize (name)
  ;; initialize a constant in memory at compile-time, if necessary
  ;; the Allocator has decided which constants are manifest for this given CPU architecture, and,
  ;; which are non-manifest, e.g. 5 is manifest, whereas "abcdef" is not manifest and needs
  ;; to be put somewhere in memory (resp: manifest=fits in a CPU register, non-manifest=does not fit in a register and needs to be put in memory)
  ;; this early version of the POC code does nothing and assumes that everything will be interpreted
  (let ((dd (lookup *synonyms* name)))
    ;; this is where you could do something with the :value of the initialized variable
    ))

(defun $ir-freshreturns ()
  (venter result))
(defun $ir-disposereturns ()
  (vexit result))


(defun $ir-createTemp (name)
  (let ((dd (lookup *synonyms* name)))
    ;; no-op in this (non-optimized) version
    (declare (ignore dd))))

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
          (format *standard-output* "~a~%" opcode)
          (cond
             ((string-equal "$g-pushScope" opcode) (apply #'$g-pushScope operands))
             ((string-equal "$g-popScope" opcode) (apply #'$g-popScope operands))
             ((string-equal "$g-defsynonym" opcode) (apply #'$g-defsynonym operands))
             ((string-equal "$a-defsynonym" opcode) (apply #'$a-defsynonym operands))
             ((string-equal "$ir-defsynonym" opcode) (apply #'$ir-defsynonym operands))
             ((string-equal "$ir-beginFunction" opcode) (apply #'$ir-beginFunction operands))
             ((string-equal "$ir-endFunction" opcode) (apply #'$ir-endFunction operands))
             ((string-equal "$ir-return-from-function" opcode) (apply #'$ir-return-from-function operands))
             ((string-equal "$ir-call" opcode) (apply #'$ir-call operands))
             ((string-equal "$ir-save-return-value" opcode) (apply #'$ir-save-return-value operands))
             ((string-equal "$ir-freshargs" opcode) (apply #'$ir-freshargs operands))
             ((string-equal "$ir-pushArg" opcode) (apply #'$ir-pushArg operands))
             ((string-equal "$ir-disposeargs" opcode) (apply #'$ir-disposeargs operands))
             ((string-equal "$ir-initialize" opcode) (apply #'$ir-initialize operands))
             ((string-equal "$ir-freshreturns" opcode) (apply #'$ir-freshreturns operands))
             ((string-equal "$ir-disposereturns" opcode) (apply #'$ir-disposereturns operands))
             ((string-equal "$ir-createTemp" opcode) (apply #'$ir-createTemp operands))
             (t (assert nil)))
          ($-run))))))

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
        *synonyms* (make-instance 'synonym-table)
))

(defun lookup-code (name)
  (gethash name *code*))

(defun bind-formals (function-descriptor arg-pairs)
  (let ((fparams (formals function-descriptor)))
    (if (not (= (length fparams) (length arg-pairs)))
        (compiler-error (format nil "wrong number of arguments passed to function ~a (given ~a)" function-descriptor arg-pairs))
      (mapc #'(lambda (param arg-pair)
                (let ((name param)
                      (val  (second arg-pair)))
                  (vput parameter name val)))
            fparams arg-pairs))))

(defun compiler-error (message)
  (error message))

(defun lookup-synonym (name)
  (lookup *synonyms* name))
