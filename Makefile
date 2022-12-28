
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
	| ./bred/bred beginfunction.bred bred \
	| ./bred/bred endfunction.bred bred \
	| ./bred/bred resetargs.bred bred \
	| ./bred/bred mutate.bred bred \
	| ./bred/bred pusharg.bred bred \
	| ./bred/bred createtemp.bred bred \
	| ./bred/bred call.bred bred \
	| ./bred/bred return.bred bred \
	| ./bred/bred pushscope.bred bred \
	| ./bred/bred popscope.bred bred \
	| cat - >ex1.lisp

lisp-run:
	sbcl --noinform --load ex1.lisp --eval '(%test)' --quit 2>/dev/null


ex1: ex1.c
	gcc -o ex1 ex1.c
