(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defun printf ()
  ;; does not attempt to implement all of printf
  ($pushNewScope *synonyms*)
  ($pushNewScope *parameters*)
  (let ((fmt-od ($s-var "char" *args* '(@ 0)))
	(first-value-od ($s-var "char" *args* '(@ 1))))
    (let ((fmt ($get fmt-od))
          (first-value ($get first-value-od)))
      (format *standard-output* "fmt-od: /~a/ val-od: ~a~%" fmt-od first-value-od)
      (format *standard-output* "fmt: /~a/~%val: ~a~%" fmt first-value)))
  ($popScope *synonyms*)
  ($popScope *parameters*))  


