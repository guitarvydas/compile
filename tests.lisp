(declaim (optimize (debug 3) (safety 3) (speed 0)))

;;;

(defun irtest4 ()
  (reset-all)
  ($g-pushScope)
  ($g-defsynonym 'identity ($g-func
			     "identity"
			     (list (list "c" "char")) ;; param - c
			     (list "char"))) ;; return type - char
    
  ($g-defsynonym 'main ($g-func
                         "main"
                         (list (list "argc" "int") (list "argv" "char**")) ;; params - argc, argv
                         (list "void"))) ;; return type - none (void)

  (stput *code* "identity" *script-identity*)
  (stput *code* "main" *script-main*)
  
  ($g-defsynonym 'printf ($g-bifunc "printf" (list "string" "varargs") (list "void")))

  ($a-defsynonym '%argc ($a-manifestconstant "int" 1))
  ($ir-pusharg '%argc)
  ($a-defsynonym '%argv ($a-initialized "char**" *globals* 0 ""))
  ($ir-pusharg '%argv)
  (format *standard-output* "~%$-run...~%")
  (script-main))

(defun irtest ()
  (irtest4))

