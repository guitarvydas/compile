(declaim (optimize (debug 3) (safety 3) (speed 0)))


(defparameter *instructions* nil)
;; *synonyms* defined in descriptor.lisp


(define-symbol-macro _ :_)


;; vm  

(defun $ir-beginFunction (name)
  (let ((function-descriptor (lookup *synonyms* name)))
    ;; emit label and function prequel ...>> (name function-descriptor)
    ;; and, bind args to params, in correct order (a3 a2 a1) -> (p1 p2 p3)
    (let ((arg-pairs (reverse (sttop-scope-as-list *arg*))))
      (bind-formals function-descriptor arg-pairs))))

(defun $ir-endFunction (name)
  (let ((function-descriptor (value name)))
    (declare (ignore function-descriptor))
    (stexit *parameter*)
    ($-dispose-temps)))

(defun $ir-return-from-function (name)
  (stpush *result* (value name))
)

(defun $ir-call (fstr)
  (funcall (intern (string-upcase fstr))))

(defun $ir-save-return-value (function-name name)
  (let ((od (lookup-synonym name)))
    (let ((function-descriptor (lookup-synonym function-name)))
      (let ((rettype (return-type function-descriptor)))
        (save od ($ir-var rettype *result* 0))))))
  
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
    (save dd "<undefined>")))

(defun $ir-call-builtin (function-descriptor)
  (let ((arg-list (reverse (sttop-scope-as-list *arg*))))
    (let ((result (apply (function-symbol function-descriptor) arg-list)))
      ($ir-return-from-function result))))
  
;; end vm

;; support code - builtin functions
;; each function must return its result as a valid data descriptor, or else!
(defun printf ()
  (let ((args (reverse (sttop-scope-as-list *arg*))))
    (let ((format-string (value (first args)))
          (var-args (mapcar #'value (rest args))))
      (apply 'format format-string var-args)
      ($g-void)))) ;; return void

    


(defun $-fresh-temps ()
  (stenter *temp*))
(defun $-dispose-temps ()
  (stexit *temp*))

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
