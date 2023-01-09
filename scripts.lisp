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
  (block script
	 ($defsynonym "identity" ($g-func
				  "identity"
				  (list (cons "c" "char")) ;; param - c
				  (list "char"))) ;; return type - char
	 
	 ;;char identity (char c) {
	 ($defsynonym "c" ($s-var "char" *parameter* "c"))
	 ($defsynonym "arg c" ($s-var "char" *args* "c"))
	 ($i-copy "arg c" >> "c")
	 ;;  return c;
	 ($defsynonym "return value" ($s-var "char" *result* "0"))
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
  (block script
	 ($defsynonym "main" ($g-func
                              "main"
                              (list (cons "argc" "int") (cons "argv" "char**")) ;; params - argc, argv
                              (list "void"))) ;; return type - none (void)
	 
	 ($defsynonym "argc" ($s-var "int" *parameter* "argc"))
	 ($defsynonym "argv" ($s-pointer "char**" *parameter* "argv"))
	 ($defsynonym "arg argc" ($i-var "int" *temp* "0"))
	 ($defsynonym "arg argv" ($i-var "int" *temp* "1"))
	 ($i-copy "arg argc" >> "argc")
	 ($i-copy "arg argv" >> "argv")
	 ;;  char x = identity ('x');
	 ($defsynonym "x" ($s-var "char" *temp* "x"))
	 ($defsynonym "undefined" ($i-var "void" _ "<undefined>"))
	 ($i-copy "undefined" >> "x")
	 ($i-push *temp* "x")
	 ($pushNewScope *args*)
	 ($pushNewScope *results*)
	 (block call-script
		($i-defsynonym "%%0" ($i-literal "char" "x"))
		($i-initializeLiteral "%%0")
		($i-push *args* "%%0")
		($i-call "script-identity")
		($defsynonym "return 0" ($s-var "char" *result* "return 0"))
		($i-copy "return 0" >> "%%0"))
	 ($popScope *results*)
	 ($popScope *args*)
	 ;;  printf ("result = %c\n", x);
	 ($g-defsynonym "PRINTF" ($g-bifunc "printf" (list (cons "fmt" "string") (cons "varargs" "%rest")) (list "void")))
	 ($i-pushNewScope *args*)
	 ($ipushNewScope *result*)
	 (block call-script
		($i-push *temp* "%%1")
		($i-copy "undefined" >> "%%1")
		($defsynonym "%%2" ($s-literal "string" *globalConstants* "%%2"
					       ((0 . #\r) 
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
		($i-push *args* "%%2")
		($i-push *args* "x") 
		($i-call "PRINTF")
		($defsynonym "result from PRINTF" ($i-var "void" *temp* "return from PRINTF 0"))
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
