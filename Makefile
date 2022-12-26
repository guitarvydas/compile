
all: lisp

repos:
	multigit -r

lisp:
	./ir2lisp.py <ex1.ircode \
	| ./bred/bred defsyn.bred bred \
	| ./bred/bred defsyn.bred bred \
	| cat -

lisp-run:
	sbcl --noinform --load ex1.lisp --eval '(%test)' --quit 2>/dev/null


ex1: ex1.c
	gcc -o ex1 ex1.c
