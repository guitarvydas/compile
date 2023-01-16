
(let ((root "/Users/tarvydas/quicklisp/local-projects/compile/"))
  (labels ((ld (fname)
             (load (format nil "~a~a" root fname))))
	  ;; basics
	  (ld "scoped-table.lisp")
	  (ld "bases.lisp")
          (cold-start)
	  (ld "synonyms.lisp")
	  (ld "descriptor.lisp")

	  ;; phases
	  (ld "gather.lisp")
	  (ld "semantics-checker.lisp")
	  (ld "i.lisp")

	  ;; built in functions
	  (ld "builtin.lisp")

	  ;; scripts
	  (ld "scripts.lisp")

          ;; tests
          (ld "tests.lisp")
))
