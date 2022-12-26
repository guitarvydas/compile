;; each allocation space is a sparse array - a stack that contains {index value} pairs
;; access of an item is a linear search for its index beginning at the top of the stack
;; "mutation" doesn't change anything, but simply pushes another pair onto the stack
;;   that contains the same index, overriding previous values at that index due to the
;;   linear search strategy (this can be later optimized to mutate values at given indices)

(defclass od ()
  ((type :accessor type :initarg :type)
   (indirection :accessor indirection :initarg :indirection)
   (base :accessor base :initarg :base)
   (index :accessor index :initarg :index)))

(defparameter *temps-stack* nil)
(defparameter *args-stack* nil)
(defparameter *parameters-stack* nil)
(defparameter *results-stack* nil)
(defparameter *pointers-stack* nil)


(setf _ '_)
(setf @1 1)

(defun od-char (k b i)
  (make-instance 'od :type 'char :indirection k :base b :index (1- i)))

(defun od-function (k b i)
  (make-instance 'od :type 'function :indirection k :base b :index i))

(defun od-void (k b i)
  (make-instance 'od :type 'void :indirection k :base b :index i))


(defparameter *script-identity* `(
			 ($ir.begin-function "identity" 1)
			 ($ir.return ,(od-char 1 'param 1))
			 ($ir.end-function "identity" 1)
			 ))

(defparameter *script-main* `(
;;int main (int argc, char **argv) {
      ($ir.beginFunction "main" 2)
;;  char x = identity ('x');
      ($ir.resetArgs)
      ($ir.mutate ,(od-char @1 temp 1) ,(od-char - temp "x"))
      ($ir.pushArg ,(fetch (od-char @1 temp 1)))
      ($ir.createTemp ,(od-char @1 temp 2))
      ($ir.defsynonym _0 ,(od-char @1 temp 2))
      ($ir.call ,(od-function 'identity _ _))
      ($ir.mutate _0 ,(od-char @1 result 1))
;;  printf ("result = %c\n", x);
      ($ir.resetArgs)
      ($ir.createTemp ,(od-char @1 temp 2))
      ($ir.defsynonym _1 ,(od-char @1 temp 2))
      ($ir.mutate ,(od-char @1 string 2) ,(od-char - temp "result = %c\n"))
      ($ir.pushArg ,(od-char @1 string 2))
      ($ir.pushArg 'x)
      ($ir.call ,(od-function 'printf _ _))
      ($ir.mutate _1 ,(od-char @1 result 1))
      ($ir.return ,(od-void _ _ _)) 
;;}
      ($ir.endFunction "main" 2)))

(defun $ir.beginFunction (name nparams)
  ($.clear-temps)
  ($.reverse-args))

(defun $ir.return (od)
  (push *results-stack* (od fetch)))

(defun $ir.endFunction (name nparams)
  (let ((n nparams))
    (loop while (> nparams 0)
	  do (pop *params-stack*))))

(defun $ir.resetArgs ()
  (setf *args-stack* nil)

(defun $ir.mutate (dest src)
  (let ((v (src fetch)))
    (dest assign v)))

(defun $ir.pushArg (v)
  (push v *args*))

(defun $ir.createTemp (od)
  ;; push {index od} pair onto temps stack
  (push `(,(displacement od) ,od) *temps-stack*))

(defun defsynonym (name od)
  ;; put the lval template od into a mapping table at "name"
  ;; for now, each entry is a stack, with the most recent entry being first and overriding
  ;;  all other entries in that slot
  (multiple-value-bind (stack success) (gethash name *synonyms*)
     (if success
	 (push od (gethash name *synonyms*))
       (setf (gethash name *synonyms*) (list od)))))

(defun $ir.call (od)
  (let ((script (fetch od)))
    (push *instruction-stack* script)))


(defun $ir.test ()
  (setf *code* `(("identity" ,*script-identity*) ("main" ,*script-main*)))
  (setf *args-stack* nil
	*temps-stack* nil
	*parameters-stack* nil
	*results-stack* nil)
  (setf *instruction-stack* (list (fetch `(od-function 'main _ _))))
  ($.run))

(defun $.clear-temps ()
  (setf *temps-stack* nil))

(defun $.reverse-args ()
  (setf *args* (reverse *args*)))

(defun $.run ()
  (if (null *instruction-stack*)
      nil
    (if (null (first *instruction-stack*))
	(progn
	  (pop *instruction-stack*)
	  ($.run))
      (let ((instruction (pop (first *instruction-stack*))))
	(funcall instruction))))))
	  

(defmethod od-fetch ((self od-char))
  (case (indirection self)
    (0 (index self))
    (1 (nth (index self) (base self)))
    (otherwise
     (let ((pointer (od-fetch 
       (od-fetch (od-char (1- (indirection self)) (base self) pointer))
  )

(defmethod od-fetch ((self od-function))
  (index self))

(defmethod od-fetch ((self od-void))
  'void)



(defun fetch (x)
  (if (symbolp x)
      ()
    (od-fetch x)))