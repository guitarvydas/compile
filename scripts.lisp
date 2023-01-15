(declaim (optimize (debug 3) (safety 3) (speed 0)))

(define-symbol-macro >> :>>)


;; scripts

;; $defsynonym - add operand and data descriptors to synonyms table
;; $g >> gather pass
;; $s >> semantic pass, type checking
;; $i >> interpreter

(defun script-identity  ()
  ($pushNewScope *synonyms*)
  ($pushNewScope *parameters*)
  ($defsynonym *synonyms* "identity" ($g-func
				      "identity"
				      (list (cons "c" "char")) ;; param - c
				      (list "char"))) ;; return type - char
  ($beginFunction ($lookup *synonyms* "identity"))
  (block script
	 ;;char identity (char c) {
	 ($defsynonym *synonyms* "arg 0" ($s-var "char" *args* "arg 0"))
	 ($defsynonym *synonyms* "c" ($s-var "char" *parameters* "c"))
	 ($copy ($get ($lookup *synonyms* "arg 0")) >> ($lookup *synonyms* "c"))
	 ;;  return c;
	 ($defsynonym *synonyms* "result value" ($s-var "char" *results* "result 0"))
	 ($copy ($get ($lookup *synonyms* "c")) >> ($lookup *synonyms* "result value"))
	 (return-from script))
  ;;}
  ($popScope *parameters*)
  ($popScope *synonyms*)
  ($endFunction "identity"))
  
(defun script-main ()
  ;; int main (int argc, char **argv) {
  ($pushNewScope *synonyms*)
  ($pushNewScope *parameters*)
  ($pushNewScope *temps*)
  ($defsynonym *synonyms* "main" ($g-func
				  "main"
				  (list (cons "argc" "int") (cons "argv" "char**")) ;; params - argc, argv
				  (list "void"))) ;; return type - none (void)
  ($beginFunction ($lookup *synonyms* "main"))
  (block script
    
    ;; map stack-relative (args 0) to named slot "argc"
    ($defsynonym *synonyms* "argc" ($s-var "int" *parameters* "argc")) ;; named
    ($defsynonym *synonyms* "arg 0" ($s-var "int" *args* '(@ 0))) ;; stack relative
    ($copy ($get ($lookup *synonyms* "arg 0")) >> ($lookup *synonyms* "argc"))
    
    ($defsynonym *synonyms* "argv" ($s-var "char**" *parameters* "argv")) ;; named
    ($defsynonym *synonyms* "arg 1" ($s-var "int" *args* '(@ 1))) ;; stack relative
    ($copy ($get ($lookup *synonyms* "arg 1")) >> ($lookup *synonyms* "argv"))
    
    ;;  char x = identity ('x');
    ($defsynonym *synonyms* "%0" ($s-literal "char" "x"))
    ($pushNewScope *args*)
    ($pushNewScope *results*)
    (block script
      ($push *args* ($get ($lookup *synonyms* "%0")))
      ($call ($lookup *synonyms* "script-identity"))
      ($defsynonym *synonyms* "return 0" ($s-var "char" *results* '(@ 0)))
      ($defsynonym *synonyms* "local x" ($s-var "char" *temps* "x"))
      ($copy ($get ($lookup *synonyms* "return 0")) >> ($lookup *synonyms* "local x"))
      (return-from script))
    ($popScope *results*)
    ($popScope *args*)
    
    ;;  printf ("result = %c\n", x);
    ($defsynonym *synonyms* "PRINTF" ($g-bifunc "printf" 
                                                (list (cons "format" "char*") (cons "varargs" "%rest")) 
                                                (list "void")))
    
    ($defsynonym *synonyms* "format[]" ($s-collection "char"))
    ($cappend ($lookup "format[]") ($s-literal "char" #\r))
    ($cappend ($lookup "format[]") ($s-literal "char" #\e))
    ($cappend ($lookup "format[]") ($s-literal "char" #\s))
    ($cappend ($lookup "format[]") ($s-literal "char" #\u))
    ($cappend ($lookup "format[]") ($s-literal "char" #\l))
    ($cappend ($lookup "format[]") ($s-literal "char" #\t))
    ($cappend ($lookup "format[]") ($s-literal "char" #\Space))
    ($cappend ($lookup "format[]") ($s-literal "char" #\=))
    ($cappend ($lookup "format[]") ($s-literal "char" #\Space))
    ($cappend ($lookup "format[]") ($s-literal "char" #\%))
    ($cappend ($lookup "format[]") ($s-literal "char" #\c))
    ($cappend ($lookup "format[]") ($s-literal "char" #\Newline))
    ($cappend ($lookup "format[]") ($s-literal "char" #\Null))
    ($defsynonym *synonyms* "* format string" ($s-literal-index ($lookup *synonyms* "format[]") 0))
    
    ($pushNewScope *args*)
    ($pushNewScope *results*)
    (block script
      ($push *args* ($get ($lookup *synonyms* "x")))
      ($push *args* ($get ($lookup *synonyms* "* format string")))
      ($call ($lookup *synonyms* "PRINTF"))
      ($defsynonym *synonyms* "return 0" ($s-var "void" *results* '(@ 0)))
      ($defsynonym *synonyms* "%0" ($s-var "char" *temps* "%0"))
      ($copy ($get ($lookup *synonyms* "return 0")) >> ($lookup *synonyms* "%0"))
      (return-from script nil))
    ($popScope *results*)
    ($popScope *args*)
    
    ($defsynonym *synonyms* "<nothing>" ($s-literal "void" nil))
    ($defsynonym *synonyms* "result value from main" ($s-var "void" *results* '(@ 0)))
    ($copy ($get ($lookup *synonyms* "<nothing>")) >> ($lookup *synonyms* "result value from main"))
    (return-from script))
  ;;}
  ($popScope *temps*)
  ($popScope *parameters*)
  ($popScope *synonyms*) 
  ($endFunction ($lookup *synonyms* "main"))
  )
