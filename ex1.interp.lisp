(defun %ex1 ()

;; #include <stdio.h>

;; char identity (char c) {
      ($ir.beginFunction “identity”)
;;   return c;
      ($ir.return '(char (dd 1 param “c”)))
      ($ir.endFunction “identity”)
;; }  

;; int main (int argc, char **argv) {
      ($ir.beginFunction “main”)
;;   char x = identity ('x');
      ($ir.resetArgs)
      ($ir.createTemp "%0" '(char (dd 1 temp "%0" "")))
      ($ir0.initialize '(char (dd 0 string 0 _)) "x")
      ($ir.assignArg 0 '(char (dd 0 string 0 _)))
      ($ir.call (dd 1 code "identity" "identity"))
      ($ir.assignReturnInto 0 '(char (dd 1 temp "%0" "")))
;;   printf ("result = %c\n", x);
      ($ir.resetArgs)
      ($ir.createTemp "%1" '(char (dd 1 temp "%1" "%1")))
      ($ir0.initialize '(char (dd 0 string 2 _)) "result = %c\n")
      ($ir.assignArg 0 '(char (dd 0 2 _)))
      ($ir.appendArg '(char (dd 1 temp "%0" "")))
      ($ir.call (dd 1 code "printf" "printf"))
      ($ir.assignReturnInto 0 '(void (dd 0 temp "%1" _)))
      ($ir.returnVoid)
;; }
      ($ir.endFunction "main")

)
