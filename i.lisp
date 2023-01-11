(declaim (optimize (debug 3) (safety 3) (speed 0)))


(defparameter *instructions* nil)
;; *synonyms* defined in descriptor.lisp


(define-symbol-macro _ nil)


;; vm  

(defun $i-beginFunction (name)
  ;; prologue code if any
  (let ((function-descriptor ($lookup *synonyms* name)))
    (declare (ignore function-descriptor))))

(defun $i-endFunction (name)
  ;; epilogue code if any
  (let ((function-descriptor (value name)))
    (declare (ignore function-descriptor))))

(defun $i-copy (operand1 sugar operand2)
  (declare (ignore sugar))
  ;; copy data from slot in operand1 into slot of operand2
  (let ((data ($get ($lookup *synonyms* operand1))))
    ($save operand2 data)))

(defun $i-return-from-function (name)
  ;; go back to caller
  (declare (ignore name))
  (throw 'script nil))

(defun $i-push (base name)
  (let ((data ($get ($lookup *synonyms* ($lookup *synonyms* name)))))
    (stpush base data)))

(defun $i-initializeLiteral (name)
  ;; if this literal needs to sit in *globalConstants*, create it
  ;; example" the string "abcdefghijklmno" probably needs to be stored in memory, whereas
  ;;  the character 'x' does not need to sit in memory (it is literally stored
  ;;  in the operand/data descriptor)
  ;; What needs to be in-memory vs. what can be stored in a register, is
  ;;  CPU-dependent
  ;; example: edge-case: the string "abcd" might fit in a register in some
  ;;  CPUs while it won't fit in a register on other CPUs and would need to
  ;;  be stored in memory in those cases
  ;; This only matters when we are trying to compile code to binary. It doesn't
  ;;  matter when we transpile code to some other language (like Lisp), in
  ;;  which case we simply punt the issue to the underlying compiler and let it
  ;;  worry about where to allocate literal data.
  (let ((data ($get ($lookup *synonyms* name))))
    (declare (ignore data))))

(defun $i-call (function-name)
  (funcall (intern (string-upcase function-name))))


