(declaim (optimize (debug 3) (safety 3) (speed 0)))

;;;

(defun itest4 ()
  (reset-all)
  ($pushNewScope *synonyms*)
  ($pushNewScope *parameters*)
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
    (let ((c ($s-collection "char[]")))
      ($cpush ($s-literal "char" ($s-literal "char" #\Null)) c)
      ($cpush ($s-literal "char" ($s-literal "char" #\t)) c)
      ($cpush ($s-literal "char" ($s-literal "char" #\s)) c)
      ($cpush ($s-literal "char" ($s-literal "char" #\e)) c)
      ($cpush ($s-literal "char" ($s-literal "char" #\T)) c)
      ($defsynonym *synonyms* "TestName" c)
      
      (let ((pointer-to-char-array ($s-literal-index ($lookup *synonyms* "TestName") 0)))
        
        (let ((collection-of-pointer-char-array ($s-collection "char[][]")))
          ($cpush pointer-to-char-array collection-of-pointer-char-array)
          
          ($defsynonym *synonyms* "argv for testing" ($s-literal-index collection-of-pointer-char-array 0))
          
          ($pushNewScope *results*)
          ($push *args* ($get ($lookup *synonyms* "argv for testing")))
          ($push *args* ($get ($lookup *synonyms* "argc")))
          (format *standard-output* "~%$-run...~%")
          (script-main)
          ($popScope *args*)
          ($popScope *results*))))
    ($popScope *parameters*)
    ($popScope *synonyms*)))

    
(defun itest ()
  (itest4))

