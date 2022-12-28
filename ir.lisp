;; each allocation space is a sparse array - a stack that contains {index value} pairs
;; access of an item is a linear search for its index beginning at the top of the stack
;; "mutation" doesn't change anything, but simply pushes another pair onto the stack
;;   that contains the same index, overriding previous values at that index due to the
;;   linear search strategy (this can be later optimized to mutate values at given indices)


(defparameter *code-stack* (vstack))
(defparameter *scope-stack* (vstack))
(defparameter *temps-stack* (vstack))
(defparameter *args-stack* (vstack))
(defparameter *parameters-stack* (vstack))
(defparameter *results-stack* (vstack))

(defparameter *instruction-stack* nil)
(defparameter *synonyms* nil)


(define-symbol-macro @0 0)
(define-symbol-macro @1 1)
(define-symbol-macro @2 2)
(define-symbol-macro _ :_)

(defclass od-char-class (od)
  ())

(defclass od-function-class (od)
  ())

(defclass od-void-class (od)
  ())

(defun od-char (k b i)
  (od 'char k b i))

(defun od-function (k b i signature)
  (od 'function k b i signature))

(defun od-void (k b i)
  (od 'void k b i))


;; scripts

(defparameter *script-identity* `(
  ($g-defsynonym identity (od-function - *code-stack* "identity" (list
                                                            (list (od-char @1 _ _)) ;; param - c
                                                            (list (od-char @1 _ _))))) ;; return type - char
    
     ($g-pushScope)
;;char identity (char c) {
     ($g-defsynonym c (od-char @1 param 1))
     ($ir-beginFunction identity) 
;;  return c;
     ($ir-return c)
;;}
     ($g-popScope)
     ($ir-endFunction identity)
			 ))

(defparameter *script-main* `(
;;int main (int argc, char **argv) {
  ($g-defsynonym main (od-function 0 *code-stack* "main" (list ;; signature
                                                    (list (od-int @1 _ _)(od-char @2 _ _)) ;; params - argc, argv
                                                    (list (od-void _ _ _))))) ;; return type - none (void)

    ($g-pushScope)
      ($g-defsynonym argc (od-int @1 param 1))
      ($g-defsynonym argv (od-char @2 param 2))
      ($ir-beginFunction main)
;;  char x = identity ('x');
      ($g-defsynonym x (od-char @1 temp 1)) 
      ($ir-resetArgs) 
      ($ir-mutate  x (od-char @0 temp "x"))  
      ($ir-pushArg x)
      ($ir-defsynonym %%0 (od-char @1 temp 2)) 
      ($ir-createTemp %%0)
      ($ir-call identity)
      ($ir-mutate  %%0 (od-char @1 result 1)) 
;;  printf ("result = %c\n", x);
      ($g-defsynonym printf (od-bifunction _ (list (od-char @1 _ _) (od-varargs _ _ _)) (list (od-void _ _ _))))
      ($ir-resetArgs) 
      ($ir-defsynonym %%1 (od-char @1 temp 2)) 
      ($ir-createTemp %%1)
      ($ir-mutate  %%1 (od-char @0 temp "result = %c\n")) 
      ($ir-pushArg %%1)
      ($ir-pushArg x) 
      ($ir-call printf) 
      ($ir-mutate  %%1 (od-char @1 result 1)) 
      ($ir-return (od-void _ _ _)) 
;;}
    ($g-popScope) 
      ($ir-endFunction main)
			      ))
  

;; vm  

(defun $g-pushScope ()
  (push nil *scope-stack*))

(defun $g-popScope ()
  (pop *scope-stack*))

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
		 (pop *parameters-stack*)
		 (decf n))))))

(defun $ir-return (od)
  (push (fetch od) *results-stack*))

(defun $ir-resetArgs ()
  (setf *args-stack* (vstack)))

(defun $ir-pushArg (v)
  (push v *args-stack*))

(defun $ir-mutate (dest src)
  (let ((v (fetch src)))
    (assign dest v)))

(defun $ir-createTemp (od)
  ;; push {index od} pair onto temps stack
  (push `(,(index od) ,od) *temps-stack*))

(defun $ir-call (od)
  (let ((script (fetch od)))
    (push *instruction-stack* script)))


;; end vm



    


(defun $-clear-temps ()
  (setf *temps-stack* nil))

(defun $-reverse-args ()
  (setf *args-stack* (reverse *args-stack*)))

(defun $-run ()
  (if (null *instruction-stack*)
      nil
    (if (null (first *instruction-stack*))
	(progn
	  (pop *instruction-stack*)
	  ($-run))
      (let ((instruction (pop (first *instruction-stack*))))
	(funcall instruction)))))

(defmethod formals ((self od))
  (assert (eq 'function (dtype self)))
  (let ((desc (description self)))
    (first desc)))

;;;

(defun reset-all ()
  (setf *code-stack* (vstack)
        *scope-stack* (vstack)
        *temps-stack* (vstack)
        *args-stack* (vstack)
        *parameters-stack* (vstack)
        *results-stack* (vstack)
        *instruction-stack* nil
        *synonyms* nil))

  
;;;
(defun irtest ()
  (reset-all)
  (let ((c (od-char 1 *temps-stack* 0)))
    (assign c #\X)
    (format *standard-output* "~a~%" (fetch c))))
