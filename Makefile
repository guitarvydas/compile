all: lisp

repos:
	multigit -r

lisp:
	./ir2lisp.py ex1.ircode >ex1.0.lisp
	./bred/bred defsyn.bred bred <ex1.0.lisp >ex1.1.lisp
	./bred/bred irdefsyn.bred bred <ex1.1.lisp

lisp-run:
	sbcl --noinform --load ex1.lisp --eval '(%test)' --quit 2>/dev/null


ex1: ex1.c
	gcc -o ex1 ex1.c
