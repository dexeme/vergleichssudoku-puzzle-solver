all: main

main: solver.pl
	swipl -o solver -c solver.pl

board:
	python3 ../Python/gerador.py

clean:
	rm -f solver
