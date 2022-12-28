;; each allocation space is a sparse array - a stack that contains {index value} pairs
;; access of an item is a linear search for its index beginning at the top of the stack
;; "mutation" doesn't change anything, but simply pushes another pair onto the stack
;;   that contains the same index, overriding previous values at that index due to the
;;   linear search strategy (this can be later optimized to mutate values at given indices)

(defclass od ()
  ((dtype :accessor type :initarg :dtype)
   (indirection :accessor indirection :initarg :indirection)
   (base :accessor base :initarg :base)
   (index :accessor index :initarg :index)))

;;; stacks of indexed values 
;;;   pairs {index value}
(defclass sparse-array ()
  ((stack :accessor stack :initform nil))

(defmethod vput ((self sparse-array) index v)
  (push (stack self) (list index v)))

(defmethod vget ((self sparse-array) index)
  (labels ((top-down-search (stack index)))
    (if (null stack)
        (assert nil)
      (let ((item (first stack)))
        (if (eq (first item) index)
            (second item)
          (top-down-search (rest stack)))))))
                      
(defun vstack ()
  (make-instance 'sparse-array))

(defparameter *code-stack* (vstack))
(defparameter *scope-stack* (vstack))
(defparameter *temps-stack* (vstack))
(defparameter *args-stack* (vstack))
(defparameter *parameters-stack* (vstack))
(defparameter *results-stack* (vstack))

(defparameter *instruction-stack* nil)


(setf _ '_)
(setf @1 1)

(defclass od-char-class (od)
  ())

(defclass od-function-class (od)
  ())

(defclass od-void-class (od)
  ())

(defun od (dtype k b i)
  (make-instance 'od :dtype 'dtype :indirection k :base b :index (1- i)))

(defun od-char (k b i)
  (od 'char k b i))

(defun od-function (k b i)
  (od 'function k b i))

(defun od-void (k b i)
  (od 'void k b i))


;; scripts

(defparameter *script-identity* `(
  ($g-defsynonym identity (od-function identity
                                   (list (od-char @1 :_ :_)) ;; param - c
                                   (list (od-char @1 :_ :_)))) ;; return type - char
    
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
  ($g-defsynonym main (od-function main
                               (list (od-int @1 :_ :_)(od-char @2 :_ :_)) ;; params - argc, argv
                               (list (od-void :_ :_ :_)))) ;; return type - none (void) 
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
      ($g-defsynonym printf (od-bifunction :_ (list (od-char @1 :_ :_) (od-varargs :_ :_ :_)) (list (od-void :_ :_ :_))))
      ($ir-resetArgs) 
      ($ir-defsynonym %%1 (od-char @1 temp 2)) 
      ($ir-createTemp %%1)
      ($ir-mutate  %%1 (od-char @0 temp "result = %c\n")) 
      ($ir-pushArg %%1)
      ($ir-pushArg x) 
      ($ir-call printf) 
      ($ir-mutate  %%1 (od-char @1 result 1)) 
      ($ir-return (od-void :_ :_ :_)) 
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
  (multiple-value-bind (stack success) (gethash name *synonyms*)
     (if success
	 (push od (gethash name *synonyms*))
       (setf (gethash name *synonyms*) (list od)))))

(defun $g-defsynonym (name od)
  (defsynonym name od))

(defun $ir-defsynonym (name od)
  (defsynonym name od))

(defun $ir-beginFunction (name)
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
  (push *results-stack* (fetch od)))

(defun $ir-resetArgs ()
  (setf *args-stack* (vstack)))

(defun $ir-pushArg (v)
  (push v *args-stack*))

(defun $ir-mutate (dest src)
  (let ((v (fetch src)))
    (dest assign v)))

(defun $ir-createTemp (od)
  ;; push {index od} pair onto temps stack
  (push `(,(index od) ,od) *temps-stack*))

(defun $ir-call (od)
  (let ((script (fetch od)))
    (push *instruction-stack* script)))


;; end vm



    


(defun $ir-test ()
  (setf *code-stack* (vstack))
  (*code-stack* vput "identity" *script-identity*) 
  (*code-stack* vput "main" *script-main*)
  (setf *args-stack* (vstack)
	*temps-stack* (vstack)
	*parameters-stack* (vstack)
	*results-stack* (vstack))
  (setf *instruction-stack* (list (fetch `(od-function 0 'main _))))
  ($-run))



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


;;;; toolbox: simplistic implementation of Operand Descriptors as sparse arrays - stacks of indexed values, no mutation, linear search from top
;;;;  of stack for first index that matches (multiple values with the same index can appear in the stack, but the most recent value with an
;;;;  index overrides all other values with that same index)

(defmethod od-fetch ((self od))
  (case (indirection self)
    (0 (index self))
    (1 (let ((v (sparse-array-get (base self) (index self)))
             (dty (dtype self)))
         (case dty
           (pointer (assert (numberp v)))
           (number (assert (numberp v)))
           (char (if (characterp v)
                     nil
                   (if (numberp v)
                       (setf v (code-char v))
                     (if (stringp v)
                         (setf v (char v 0))
                       (assert nil)))))
           (void (setf v 'void))
           (function (assert (symbolp v)))
           (otherwise (assert nil)))
         v))
    (otherwise
     (let ((pointer (fetch (od :dtype 'pointer :indirection 1 :base (base self) :index (index self)))))
       (fetch (od :dtype (dtype self) :indirection (1- (indirection self)) :base (base self) :index pointer))))))

(defun fetch (x)
  (if (symbolp x)
      ()
    (od-fetch x)))

(defmethod assign ((self od) v)
  (assert (not (equal (dtype self) 'void)))
  (case (indirection self)
    (0 (setf (index self) v))
    (1 (vput (base self) (index self) v))
    (otherwise
     (let ((pointer (fetch (od :dtype 'pointer :indirection 1 :base (base self) :index (index self)))))
       (let ((obj (od (dtype self) (1- (indirection self) (base self) pointer))))
         (assign obj v))))))


(defmethod assign ((self od-void-class) v)
  (error "cannot assign to void"))
