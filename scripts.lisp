(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; scripts

(defparameter *script-identity* `(
  ($g-defsynonym identity ,(funcod
			     "identity"
			     (list "char") ;; param - c
			     (list "char"))) ;; return type - char
    
     ($g-pushScope)
;;char identity (char c) {
     ($g-defsynonym c ,(vardd "char" parameter 1))
     ($ir-beginFunction identity) 
;;  return c;
     ($ir-return-from-function c)
;;}
     ($g-popScope)
     ($ir-endFunction identity)
     ))

(defparameter *script-main* `(
;; int main (int argc, char **argv) {
  ($g-defsynonym main ,($g-func
                         "main"
                         (list "int" "char**") ;; params - argc, argv
                         (list "void"))) ;; return type - none (void)

    ($g-pushScope)
      ($g-defsynonym argc ,($a-var "int" parameter 0))
      ($g-defsynonym argv ,($a-pointer "char**" parameter 1))
      ($ir-beginFunction main)
;;  char x = identity ('x');
      ($a-defsynonym x ,($a-var "char" temp 0))
      ($ir-createTemp x)
      ($ir-freshargs)
       ($ir-defsynonym %%0 ,($a-manifestconstant "char" "x"))
       ($ir-initialize %%0)
       ($ir-pushArg %%0)
         ($ir-freshreturns)
          ($ir-call identity)
          ($ir-save-return-value identity %%0)
         ($ir-disposereturns)
      ($ir-disposeargs)
;;  printf ("result = %c\n", x);
      ($g-defsynonym printf ,($g-bifunc "printf" (list "string" "varargs") (list "void")))
      ($ir-freshargs)
       ($ir-defsynonym %%1 ,($a-var "char" temp 1))
       ($ir-createTemp %%1)
       ($ir-defsynonym %%2 ,($a-initialized "string" *globals* 0 "result = %c\n"))
       ($ir-initialize %%2)
       ($ir-pushArg %%2)
       ($ir-pushArg x) 
        ($ir-freshreturns)
         ($ir-call printf)
         ($ir-save-return-value printf %%1)
        ($ir-disposereturns)
      ($ir-disposeargs)
      ($ir-return-from-function ,($g-void))
;;}
    ($g-popScope) 
      ($ir-endFunction main)
      ))

;; show as # markdown
