;; each allocation space is a sparse array - a stack that contains {index value} pairs
;; access of an item is a linear search for its index beginning at the top of the stack
;; "mutation" doesn't change anything, but simply pushes another pair onto the stack
;;   that contains the same index, overriding previous values at that index due to the
;;   linear search strategy (this can be later optimized to mutate values at given indices)


(defparameter *globals* (vstack))
(defparameter *code* (vstack))
(defparameter *scope* (vstack))
(defparameter temp (vstack))
(defparameter arg (vstack))
(defparameter parameter (vstack))
(defparameter result (vstack))

(defparameter *instructions* nil)
(defparameter *synonyms* nil)


(define-symbol-macro _ :_)

varod == variable-od
constod == constant od
pointerod == pointer-od
voidod = void od
funcod == function od
bifuncod = built-in function od

;; scripts

(defparameter *script-identity* `(
  ($g-defsynonym identity (funcod
			     "identity"
			     (list "char") ;; param - c
			     (list "char"))) ;; return type - char
    
     ($g-pushScope)
;;char identity (char c) {
     ($g-defsynonym c (varod "char" parameters 1))
     ($ir-beginFunction identity) 
;;  return c;
     ($ir-return c)
;;}
     ($g-popScope)
     ($ir-endFunction identity)
     ))

(defparameter *script-main* `(
;;int main (int argc, char **argv) {
  ($g-defsynonym main (funcod
                         "main"
                         (list "int" "char**") ;; params - argc, argv
                         (list "void")) ;; return type - none (void)

    ($g-pushScope)
      ($g-defsynonym argc (varod "int" parameter 0))
      ($g-defsynonym argv (pointerod "char**" parameter 1))
      ($ir-beginFunction main)
;;  char x = identity ('x');
      ($g-defsynonym x (varod "char" temp 0)) 
      ($ir-resetArgs) 
      ($ir-createConstant %%0 (constod "char" "x"))
      ($ir-pushArg %%0)
      ($ir-defsynonym %%0 (varod "char" temp 1)) 
      ($ir-call identity)
      (save %%0 (varod "char" results)) 
;;  printf ("result = %c\n", x);
      ($g-defsynonym printf (bifuncod "printf" (list "string" "varargs") (list "void")))
      ($ir-resetArgs) 
      ($ir-defsynonym %%1 (varod "char" temp 1)) 
      ($ir-createTemp %%1)
      ($ir-createConstant %%2 (constod "string" "result = %c\n"))
      ($ir-pushArg %%2)
      ($ir-pushArg x) 
      ($ir-call printf) 
      (save  %%1 (constod "char" result 0))
      ($ir-return (voidod))
;;}
    ($g-popScope) 
      ($ir-endFunction main)
			      ))
  

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

(defun $ir-beginFunction (name)
  (declare (ignore name))
  ($-clear-temps)
  ($-reverse-args))

(defun $ir.endFunction (name)
  (let ((function-descriptor (fetch name)))
    (let ((n (length (formals function-descriptor))))
      (loop while (> n 0)
	    do (progn
		 (pop *parameters*)
		 (decf n))))))

(defun $ir-return (od)
  (push (fetch od) *results*))

(defun $ir-resetArgs ()
  (setf *args* (vstack)))

(defun $ir-pushArg (v)
  (push v *args*))

(defun $ir-mutate (dest src)
  (let ((v (fetch src)))
    (assign dest v)))

(defun $ir-createTemp (od)
  ;; push {index od} pair onto temps stack
  (push `(,(index od) ,od) *temps*))

(defun $ir-call (od)
  (let ((script (fetch od)))
    (push *instructions* script)))


;; end vm



    


(defun $-clear-temps ()
  (setf *temps* nil))

(defun $-reverse-args ()
  (setf *args* (reverse *args*)))

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
        *scope* (vstack)
        temp (vstack)
        arg (vstack)
        parameter (vstack)
        result (vstack)
        *instructions* nil
        *synonyms* nil))

  
;;;
(defun irtest0 ()
  (reset-all)
  (let ((c (od-char 1 *temps* 0)))
    (assign c #\X)
    (format *standard-output* "~a~%" (fetch c))))

(defun irtest1 ()
  (reset-all)
  ;; create a character A in the globals area, at index 0
  (let ((c (od-char 1 *globals* 0)))
    (assign c #\A)
    (format *standard-output* "~a~%" (fetch c))
    ;; make a pointer to the character A, the pointer is in the globals area, too, at index 1
    (let ((p (od-pointer 1 *globals* 1)))
      (assign p c)
      (format *standard-output* "*p = ~a~%" (fetch p)))))


(defun irtest ()
  (irtest1))
