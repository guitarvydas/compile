(declaim (optimize (debug 3) (safety 3) (speed 0)))


(defparameter *instructions* nil)
;; *synonyms* defined in descriptor.lisp


(define-symbol-macro _ :_)

(defun $ir-var (typename space offset)
  ($a-var typename space offset))


;; vm  

(defun $g-pushScope ()
  (stenter *synonyms*))

(defun $g-popScope ()
  (stexit *synonyms*))

(defun $g-defsynonym (name od)
  (format *standard-output* " ~a" name)
  (stput *synonyms* name od))

(defun $ir-defsynonym (name od)
  (format *standard-output* " ~a" name)
  (stput *synonyms* name od))

(defun $ir-pushParameterScope ()
  (stenter *parameter*))

(defun $ir-popParameterScope ()
  (stexit *parameter*))

(defun $ir-beginFunction (name)
  (let ((function-descriptor (lookup *synonyms* name)))
    ;; emit label and function prequel ...>> (name function-descriptor)
    ;; and, bind args to params, in correct order (a3 a2 a1) -> (p1 p2 p3)
    (let ((arg-pairs (reverse (sttop-scope-as-list *arg*))))
      (stenter *parameter*)
      (bind-formals function-descriptor arg-pairs)
      ($-fresh-temps))))

(defun $ir-endFunction (name)
  (let ((function-descriptor (value name)))
    (declare (ignore function-descriptor))
    (stexit *parameter*)
    ($-dispose-temps)))

(defun $ir-return-from-function (od)
  (stpush *result* (value od)))

(defun $ir-call (name)
  (format *standard-output* " ~a" name)
  (let ((function-descriptor (lookup-synonym name)))
    (if (builtinp function-descriptor)
        ($ir-call-builtin function-descriptor)
      (let ((script (lookup-code (function-name function-descriptor))))
        (push script *instructions*)))))

(defun $ir-save-return-value (function-name od)
  (let ((function-descriptor (lookup-synonym function-name)))
    (let ((rettype (return-type function-descriptor)))
      (save od ($ir-var rettype *result* 0)))))
  
(defun $ir-freshargs ()
  (stenter *arg*))
(defun $ir-pushArg (v)
  (let ((d (lookup *synonyms* v)))
    (stpush *arg* d)))
(defun $ir-disposeargs ()
  (stexit *arg*))

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
  (stenter *result*))
(defun $ir-disposereturns ()
  (stexit *result*))


(defun $ir-createTemp (name)
  (let ((dd (lookup *synonyms* name)))
    ;; no-op in this (non-optimized) version
    (declare (ignore dd))))

(defun $ir-call-builtin (function-descriptor)
  (let ((arg-list (reverse (sttop-scope-as-list *arg*))))
    (let ((result (apply (function-symbol function-descriptor) arg-list)))
      ($ir-return-from-function result))))
  
;; end vm

;; support code - builtin functions
;; each function must return its result as a valid data descriptor, or else!
(defun printf (&rest args)
  (let ((format-string (value (first args)))
        (var-args (mapcar #'value (rest args))))
    (apply 'format format-string var-args)
    ($g-void))) ;; return void

    


(defun $-fresh-temps ()
  (stenter *temp*))
(defun $-dispose-temps ()
  (stexit *temp*))

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
          (format *standard-output* "~a" opcode)
          (cond
             ((string-equal "$g-pushScope" opcode) (apply #'$g-pushScope operands))
             ((string-equal "$ir-pushParameterScope" opcode) (apply #'$ir-pushParameterScope operands))
             ((string-equal "$g-popScope" opcode) (apply #'$g-popScope operands))
             ((string-equal "$ir-popParameterScope" opcode) (apply #'$ir-popParameterScope operands))
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
          (format *standard-output* "~%")
          ($-run))))))

;;;


(defun lookup-code (name)
  (stget *code* name))

(defun bind-formals (function-descriptor args)
  (let ((fparams (formals function-descriptor)))
    (if (not (= (length fparams) (length args)))
        (compiler-error (format nil "wrong number of arguments passed to function ~a (given ~a)" function-descriptor args))
      (mapc #'(lambda (param-pair arg)
                (let ((name (formal-name param-pair))
                      (ty (formal-type param-pair))
                      (val arg))
                  (declare (ignore ty)) ;; not used in this POC
                  (stput *parameter* name val)))
            fparams args))))

(defun compiler-error (message)
  (error message))

(defun lookup-synonym (name)
  (lookup *synonyms* name))
