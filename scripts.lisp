(declaim (optimize (debug 3) (safety 3) (speed 0)))

(define-symbol-macro >> :>>)


;; scripts

;; $defsynonym - add operand and data descriptors to synonyms tabl
;; $g >> gather pass
;; $s >> semantic pass, type checking
;; $i >> interpreter

(defun script-identity  ()
  ($i-beginFunction "identity") 
  ($pushNewScope *synonyms*)
  ($pushNewScope *parameters*)
  ($pushNewScope *temps*)
  (catch 'script
	 ($defsynonym *synonyms* "identity" ($g-func
				  "identity"
				  (list (cons "c" "char")) ;; param - c
				  (list "char"))) ;; return type - char
	 
	 ;;char identity (char c) {
	 ($defsynonym *synonyms* "c" ($s-var "char" *parameters* "c"))
	 ($defsynonym *synonyms* "arg c" ($s-var "char" *args* "c"))
	 ($i-copy "arg c" >> "c")
	 ;;  return c;
	 ($defsynonym *synonyms* "return value" ($s-var "char" *results* "0"))
	 ($i-copy "c" >> "return value")
	 ($i-return-from-function "identity"))
  ;;}
  ($popScope *temps*)
  ($popScope *parameters*)
  ($popScope *synonyms*)
  ($i-endFunction "identity"))
  
(defun script-main ()
  ;; int main (int argc, char **argv) {
  ($i-beginFunction "main")
  ($pushNewScope *synonyms*)
  ($pushNewScope *parameters*)
  ($pushNewScope *temps*)
  (catch 'script
	 ($defsynonym *synonyms* "main" ($g-func
                              "main"
                              (list (cons "argc" "int") (cons "argv" "char**")) ;; params - argc, argv
                              (list "void"))) ;; return type - none (void)
	 
	 ($defsynonym *synonyms* "argc" ($s-var "int" *parameters* "argc"))
	 ($defsynonym *synonyms* "argv" ($s-var "char**" *parameters* "argv"))
	 ;;  char x = identity ('x');
	 ($defsynonym *synonyms* "x" ($s-var "char" *temps* "x"))
	 ($defsynonym *synonyms* "undefined" ($s-literal "void" "<undefined>"))
	 ($i-copy "undefined" >> "x")
	 ($push *temps* ($get ($lookup *synonyms* "x")))
	 ($pushNewScope *args*)
	 ($pushNewScope *results*)
	 (catch 'call-script
		($defsynonym *synonyms* "%%0" ($s-literal "char" "x"))
		($i-initializeLiteral "%%0")
		($push *args* ($get ($lookup *synonyms* "%%0")))
		($i-call "script-identity")
		($defsynonym *synonyms* "return 0" ($s-var "char" *results* "return 0"))
		($i-copy "return 0" >> "%%0"))
	 ($popScope *results*)
	 ($popScope *args*)
	 ;;  printf ("result = %c\n", x);
	 ($defsynonym *synonyms* "PRINTF" ($g-bifunc "printf" (list (cons "fmt" "string") (cons "varargs" "%rest")) (list "void")))
	 ($pushNewScope *args*)
	 ($pushNewScope *results*)
	 (catch 'call-script
		($push *temps* ($get ($lookup *synonyms* "%%1")))
		($i-copy "undefined" >> "%%1")
		($defsynonym *synonyms* "%%2" ($s-literal "string" 
					       '((0 . #\r) 
                                                 (1 . #\e)
                                                 (2 . #\s) 
                                                 (3 . #\u)
                                                 (4. #\l)
                                                 (5. #\t)
                                                 (6 . #\Space)
                                                 (7 . #\=)
                                                 (8 . #\Space)
                                                 (9 . #\%)
                                                 (10 . #\c)
                                                 (11 . #\Newline)
                                                 (12 . #\Null))))
		($i-initializeLiteral "%%2")
		($push *args* ($get ($lookup *synonyms* "%%2")))
		($push *args* ($get ($lookup *synonyms* "x")))
		($i-call "PRINTF")
		($defsynonym *synonyms* "result from PRINTF" ($s-var "void" *temps* "return from PRINTF 0"))
		($i-copy "return value" >> "result from PRINTF"))
	 ($popScope *results*)
	 ($popScope *args*)
	 ($i-return-from-function ($g-void)))
  ;;}
  ($popScope *temps*)
  ($popScope *parameters*)
  ($popScope *synonyms*) 
  ($i-endFunction "main")
  )
