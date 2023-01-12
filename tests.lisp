(declaim (optimize (debug 3) (safety 3) (speed 0)))

;;;

(defun itest4 ()
  (reset-all)
  ($pushNewScope *synonyms*)
  ($pushNewScope *parameters*)
  ($pushNewScope *temps*)
  (block script
    ($defsynonym *synonyms* "identity" ($g-func
			     "identity"
			     (list (list "c" "char")) ;; param - c
			     (list "char"))) ;; return type - char
    
    ($defsynonym *synonyms* "main" ($g-func
                         "main"
                         (list (list "argc" "int") (list "argv" "char**")) ;; params - argc, argv
                         (list "void"))) ;; return type - none (void)
    
    ($defsynonym *synonyms* "PRINTF" ($g-bifunc "printf" (list "string" "varargs") (list "void")))
    
    ($defsynonym *synonyms* "%argc" ($s-literal "int" 1))
    (let ((c (make-instance 'Collection)))
      (cpush c #\Null)
      (cpush c #\t)
      (cpush c #\s)
      (cpush c #\e)
      (cpush c #\T)
      ($defsynonym *synonyms* "TestName" ($s-literal "char[]" st))
      ($defsynonym *synonyms* "ixTestName" ($s-literal-index ($get ($lookup *synonyms* "TestName")) 0))
      ($defsynonym *synonyms* "*TestName" ($s-var "pointer" *temps* "*TestName"))
      ($i-copy "ixTestName" >> "*TestName")    
      ($push *temps* ($get ($lookup *synonyms* "*TestName")))
      
      ($defsynonym *synonyms* "ix*TestName" ($s-literal-index ($get ($lookup *synonyms* "*TestName")) 0))
      ($defsynonym *synonyms* "**TestName" ($s-var "pointer[]" *args* "*TestName"))
      ($i-copy "ix*TestName" >> "**TestName")    
      
      ($pushNewScope *results*)
      (block call-script
        ($push *args* ($get ($lookup *synonyms* "%argc")))
        ($push *args* ($get ($lookup *synonyms* "**TestName")))
        (format *standard-output* "~%$-run...~%")
        (script-main))
      ($popScope *args*)
      ($popScope *results*)))
  ($popScope *temps*)
  ($popScope *parameters*)
  ($popScope *synonyms*))

    
(defun itest ()
  (itest4))

