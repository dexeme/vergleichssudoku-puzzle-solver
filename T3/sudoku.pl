:- use_module(library(clpfd)).
:- use_module(library(readutil)).

% Verifica se um número é válido em uma linha
isRowValid(Board) :-
    maplist(all_distinct, Board).

% Verifica se um número é válido em uma coluna
isColValid(Board) :-
    transpose(Board, Columns),
    maplist(all_distinct, Columns).

% Função para verificar se todos os blocos do tabuleiro são válidos
isBoxValid(9, Board) :-
    Board = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    blocks_3x3(As, Bs, Cs),
    blocks_3x3(Ds, Es, Fs),
    blocks_3x3(Gs, Hs, Is).
isBoxValid(6, Board) :-
    Board = [As,Bs,Cs,Ds,Es,Fs],
    blocks_3x2(As, Bs, Cs),
    blocks_3x2(Ds, Es, Fs).
isBoxValid(4, Board) :-
    Board = [As,Bs,Cs,Ds],
    blocks_2x2(As, Bs),
    blocks_2x2(Cs, Ds).

% Função para definir os blocos do Sudoku 9x9
blocks_3x3([], [], []).
blocks_3x3([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
    all_distinct([A,B,C,D,E,F,G,H,I]),
    blocks_3x3(Bs1, Bs2, Bs3).

% Função para definir os blocos do Sudoku 6x6
blocks_3x2([], [], []).
blocks_3x2([A,B|As1], [C,D|As2], [E,F|As3]) :-
    all_distinct([A,B,C,D,E,F]),
    blocks_3x2(As1, As2, As3).

% Função para definir os blocos do Sudoku 4x4
blocks_2x2([], []).
blocks_2x2([A,B|As], [C,D|Bs]) :-
    all_distinct([A,B,C,D]),
    blocks_2x2(As, Bs).

% Função para resolver o Sudoku
solve(Board) :-
    length(Board, Size),
    maplist(same_length(Board), Board),
    append(Board, Vs),
    Vs ins 1..Size,
    isRowValid(Board),
    isColValid(Board),
    isBoxValid(Size, Board).

% Lê o tabuleiro de um arquivo
read_board_from_file(FileName, Board) :-
    read_file_to_string(FileName, String, []),
    term_string(Board, String).

% Função principal
:- initialization(main).
main :-
    read_board_from_file('sudoku.txt', Board),
    (   solve(Board) -> maplist(writeln, Board);
        writeln('No solution found')
    ).
