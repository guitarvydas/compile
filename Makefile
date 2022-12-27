
all: lisp-test0aa

repos:
	multigit -r

lisp-test0aa:
	grep -v '$s\.' <ex0a.ircode >/tmp/test0aa
	./ir2lisp.py </tmp/test0aa >/tmp/test0a
	cat /tmp/test0a

lisp-test0a:
	./bred/db-bred ex0a.bred bred </tmp/test0a
	| cat -

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
