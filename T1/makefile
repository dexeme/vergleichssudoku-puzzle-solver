all: main

main: solver.hs
	ghc -o solver solver.hs

run:
	./solver

board:
	python3 ../Python/gerador.py

clean:
	rm -f solver *.hi *.o
