all: lisp

lisp:
	./ir2lisp.py ex1.ircode >ex1.lisp

lisp-run:
	sbcl --noinform --load ex1.lisp --eval '(%test)' --quit 2>/dev/null


ex1: ex1.c
	gcc -o ex1 ex1.c
