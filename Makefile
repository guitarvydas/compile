
all: lisp-test0

repos:
	multigit -r

lisp-test0:
	grep -v '$s\.' <ex0.ircode \
	| ./ir2lisp.py \
	| ./bred/db-bred test.bred bred \
	| cat -

lisp:
	grep -v '$s\.' <ex1.ircode \
	| ./ir2lisp.py \
	| ./bred/bred defsyn.bred bred \
	| ./bred/bred irdefsyn.bred bred \
	| cat -

lisp-run:
	sbcl --noinform --load ex1.lisp --eval '(%test)' --quit 2>/dev/null


ex1: ex1.c
	gcc -o ex1 ex1.c
