
(let ((root "/Users/tarvydas/quicklisp/local-projects/compile/"))
  (labels ((ld (fname)
             (load (format nil "~a~a" root fname))))
    (ld "vstack.lisp")
    (ld "synonyms.lisp")
    (ld "od.lisp")
    (ld "scripts.lisp")
    (ld "ir.lisp")))
