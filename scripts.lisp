(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; scripts

(defun script-identity  ()
  ($g-defsynonym 'identity ($g-func
			    "identity"
			    (list (cons "c" "char")) ;; param - c
			    (list "char"))) ;; return type - char
  
  ($g-pushScope)
  ;;char identity (char c) {
  ($ir-pushParameterScope)
  ($ir-beginFunction 'identity) 
  ($a-defsynonym 'c ($a-var "char" *parameter* "c"))
  ;;  return c;
  ($ir-return-from-function 'c)
  ;;}
  ($ir-popParameterScope)
  ($g-popScope)
  ($ir-endFunction 'identity)
  )

(defun script-main ()
  ;; int main (int argc, char **argv) {
  ($g-defsynonym 'main ($g-func
                        "main"
                        (list (cons "argc" "int") (cons "argv" "char**")) ;; params - argc, argv
                        (list "void"))) ;; return type - none (void)
  
  ($g-pushScope)
  ($ir-pushParameterScope)
  ($ir-beginFunction 'main)
  ($g-defsynonym 'argc ($a-var "int" *parameter* "argc"))
  ($g-defsynonym 'argv ($a-pointer "char**" *parameter* "argv"))
  ;;  char x = identity ('x');
  ($a-defsynonym 'x ($a-var "char" *temp* "x"))
  ($ir-createTemp 'x)
  ($ir-freshargs)
  ($ir-defsynonym '%%0 ($a-manifestconstant "char" "x"))
  ($ir-initialize '%%0)
  ($ir-pushArg '%%0)
  ($ir-freshreturns)
  ($ir-call 'identity)
  ($ir-save-return-value 'identity '%%0)
  ($ir-disposereturns)
  ($ir-disposeargs)
  ;;  printf ("result = %c\n", x);
  ($g-defsynonym 'printf ($g-bifunc "printf" (list (cons "fmt" "string") (cons "varargs" "%rest")) (list "void")))
  ($ir-freshargs)
  ($a-defsynonym '%%1 ($a-var "char" *temp* 1))
  ($ir-createTemp '%%1)
  ($a-defsynonym '%%2 ($a-initialized "string" *globals* "%%2" "result = %c\n"))
  ($ir-initialize '%%2)
  ($ir-pushArg '%%2)
  ($ir-pushArg 'x) 
  ($ir-freshreturns)
  ($ir-call 'printf)
  ($ir-save-return-value 'printf '%%1)
  ($ir-disposereturns)
  ($ir-disposeargs)
  ($ir-return-from-function ($g-void))
  ;;}
  ($ir-popParameterScope)
  ($g-popScope) 
  ($ir-endFunction 'main)
  )

;; show as # markdown
