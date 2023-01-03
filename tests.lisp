(declaim (optimize (debug 3) (safety 3) (speed 0)))

;;;

(defun irtest4 ()
  (reset-all)
  ($g-pushScope)
  ($g-defsynonym 'identity ($g-func
			     "identity"
			     (list "char") ;; param - c
			     (list "char"))) ;; return type - char
    
  ($g-defsynonym 'main ($g-func
                         "main"
                         (list "int" "char**") ;; params - argc, argv
                         (list "void"))) ;; return type - none (void)

  (vput *code* "identity" *script-identity*)
  (vput *code* "main" *script-main*)
  (push *script-main* *instructions*)
  ($a-defsynonym '%argc ($a-manifestconstant "int" 1))
  ($ir-pusharg '%argc)
  ($a-defsynonym '%argv ($a-initialized "char**" *globals* 0 ""))
  ($ir-pusharg '%argv)
  ($-run))

(defun irtest ()
  (irtest4))

