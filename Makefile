
all: lisp

repos:
	multigit -r

npmstuff:
	npm install ohm-js yargs atob pako
	npm install cli
	npm install js-beautify
lisp:
	./rmsem.py <ex1.ircode \
	| ./ir2lisp.py \
	| ./bred/bred defsyn.bred bred \
	| ./bred/bred irdefsyn.bred bred \
	| cat -

lisp-run:
	sbcl --noinform --load ex1.lisp --eval '(%test)' --quit 2>/dev/null


ex1: ex1.c
	gcc -o ex1 ex1.c
