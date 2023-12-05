:- use_module(library(readutil)).
:- use_module(library(clpfd)).

% Estrutura de uma célula
cell(Value, Right, Up, Left, Down).

% Estrutura de um tabuleiro 9x9
board9x9(Board) :-
    length(Board, 9),
    maplist(length_(9), Board).

% Estrutura de um tabuleiro 4x4
board4x4(Board) :-
    length(Board, 4),
    maplist(length_(4), Board).

% Estrutura de um tabuleiro 6x6
board6x6(Board) :-
    length(Board, 6),
    maplist(length_(6), Board).

% Auxiliar para definir o tamanho das listas internas
length_(Length, List) :- length(List, Length).

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

% Verifica se um número é válido considerando as comparações
isComparativeValid(Board, Num, Row, Col) :-
    nth0(Row, Board, RowList),
    nth0(Col, RowList, (_, Left, Up, Right, Down)),
    (Col =:= 0 -> LeftVal = 0 ; nth0(Col-1, RowList, (_, LeftVal, _, _, _))),
    (Row =:= 0 -> UpVal = 0 ; nth0(Row-1, Board, UpperRow), nth0(Col, UpperRow, (_, UpVal, _, _, _))),
    length(RowList, NumCols),
    (Col =:= NumCols-1 -> RightVal = 0 ; nth0(Col+1, RowList, (_, RightVal, _, _, _))),
    length(Board, NumRows),
    (Row =:= NumRows-1 -> DownVal = 0 ; nth0(Row+1, Board, LowerRow), nth0(Col, LowerRow, (_, DownVal, _, _, _))),
    checkLeft(Col, LeftVal, Left, Num) ,
    checkUp(Row, UpVal, Up, Num) ,
    checkRight(Col, NumCols, RightVal, Right, Num) ,
    checkDown(Row, NumRows, DownVal, Down, Num).

% Funções para verificar as comparações em todas as direções
checkLeft(Col, LeftVal, '<', Num) :- Num < LeftVal, !.
checkLeft(Col, LeftVal, '>', Num) :- Num > LeftVal, !.
checkLeft(_, 0, _, _).
checkLeft(_, _, _, _).

checkUp(Row, UpVal, '<', Num) :- Num < UpVal, !.
checkUp(Row, UpVal, '>', Num) :- Num > UpVal, !.
checkUp(_, 0, _, _).
checkUp(_, _, _, _).

checkRight(Col, NumCols, RightVal, '<', Num) :- Num < RightVal, !.
checkRight(Col, NumCols, RightVal, '>', Num) :- Num > RightVal, !.
checkRight(Col, NumCols, 0, _, _).
checkRight(Col, NumCols, _, _, _).

checkDown(Row, NumRows, DownVal, '<', Num) :- Num < DownVal, !.
checkDown(Row, NumRows, DownVal, '>', Num) :- Num > DownVal, !.
checkDown(Row, NumRows, 0, _, _).
checkDown(Row, NumRows, _, _, _).

/* Funções I/O*/

% Lê o tabuleiro de um arquivo e atribui a uma estrutura
read_board_from_file(FileName, BoardStructure) :-
    read_file_to_string(FileName, String, []),
    term_string(RawBoard, String),
    determine_board_structure(RawBoard, BoardStructure).

% Determina qual estrutura de tabuleiro usar baseado no tamanho
determine_board_structure(Board, StructuredBoard) :-
    length(Board, Size),
    (   Size == 9 -> board9x9(Board), StructuredBoard = Board;
        Size == 4 -> board4x4(Board), StructuredBoard = Board;
        Size == 6 -> board6x6(Board), StructuredBoard = Board;
        fail % falha se não for um tamanho conhecido
    ).

% Exibe uma célula
print_cell(cell(Value, _, _, _, _)) :-
    write(Value), write(' ').

% Exibe o tabuleiro
print_board([]).
print_board([Row|Rows]) :-
    maplist(print_cell, Row),
    nl,
    print_board(Rows).


% Função para resolver o Sudoku
solve(Board) :-
    length(Board, Size),
    maplist(same_length(Board), Board),
    append(Board, Vs),
    Vs ins 1..Size,
    isRowValid(Board),
    isColValid(Board),
    isBoxValid(Size, Board).

% Função principal
:- initialization(main).
main :-
    % Substituir com o caminho do seu arquivo
    read_board_from_file('../tabuleiro.txt', Board),
    (   
        solve(Board, Solution) -> print_board(Solution);
        writeln('No solution found')
    ).
