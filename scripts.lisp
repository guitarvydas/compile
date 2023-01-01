(declaim (optimize (debug 3) (safety 3) (speed 0)))

;; scripts

(defparameter *script-identity* `(
  ($g-defsynonym identity ,(funcod
			     "identity"
			     (list "char") ;; param - c
			     (list "char"))) ;; return type - char
    
     ($g-pushScope)
;;char identity (char c) {
     ($g-defsynonym c ,(varod "char" parameter 1))
     ($ir-beginFunction identity) 
;;  return c;
     ($ir-return-from-function c)
;;}
     ($g-popScope)
     ($ir-endFunction identity)
     ))

(defparameter *script-main* `(
;; int main (int argc, char **argv) {
  ($g-defsynonym main ,(funcod
                         "main"
                         (list "int" "char**") ;; params - argc, argv
                         (list "void"))) ;; return type - none (void)

    ($g-pushScope)
      ($g-defsynonym argc ,(varod "int" parameter 0))
      ($g-defsynonym argv ,(pointerod "char**" parameter 1))
      ($ir-beginFunction main)
;;  char x = identity ('x');
      ($g-defsynonym x ,(varod "char" temp 0))
      ($ir-freshargs)
       ($ir-defsynonym %%0 ,(manifestconstantod "char" "x"))
       ($ir-initialize %%0)
       ($ir-pushArg %%0)
         ($ir-freshreturns)
          ($ir-call identity)
          ($ir-save-return-value identity %%0)
         ($ir-disposereturns)
      ($ir-disposeargs)
;;  printf ("result = %c\n", x);
      ($g-defsynonym printf ,(bifuncod "printf" (list "string" "varargs") (list "void")))
      ($ir-freshargs)
       ($ir-defsynonym %%1 ,(varod "char" temp 1))
       ($ir-createTemp %%1)
       ($ir-defsynonym %%2 ,(initializedod "string" *globals* 0 "result = %c\n"))
       ($ir-initialize %%2)
       ($ir-pushArg %%2)
       ($ir-pushArg x) 
        ($ir-freshreturns)
         ($ir-call printf)
         ($ir-save-return-value printf %%1)
        ($ir-disposereturns)
      ($ir-disposeargs)
      ($ir-return-from-function ,(voidod))
;;}
    ($g-popScope) 
      ($ir-endFunction main)
      ))

;; show as # markdown
