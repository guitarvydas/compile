
(let ((root "/Users/tarvydas/quicklisp/local-projects/compile/"))
  (labels ((ld (fname)
             (load (format nil "~a~a" root fname))))
	  ;; basics
	  (ld "scoped-table.lisp")
	  (ld "synonyms.lisp")
	  (ld "bases.lisp")
	  (ld "descriptor.lisp")

	  ;; phases
	  (ld "gather.lisp")
	  (ld "semantics-checker.lisp")
	  (ld "alloc.lisp")
	  (ld "ir.lisp")

	  ;; scripts
	  (ld "scripts.lisp")

          ;; tests
          (ld "tests.lisp")
))
