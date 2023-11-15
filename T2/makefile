all: main

main: solver.lisp
	clisp -c solver.lisp

run:
	clisp solver.fas

board:
	python3 ../Python/gerador.py

clean:
	rm -f solver *.fas *.lib
