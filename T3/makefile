all: main

main: solver.pl
	swipl -o solver -c solver.pl

run:
	./solver

clean:
	rm -f solver

teste:
	swipl -o sudoku -c sudoku.pl && rm -f sudoku

board:
	python3 ../Python/gerador.py
