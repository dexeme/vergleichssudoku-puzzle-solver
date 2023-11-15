% Definição dos tipos de dados
:- dynamic cell/5.
:- dynamic board/1.

% Funções para obter elementos de uma célula
firstElem((X, _, _, _, _), X).
getSecond((_, X, _, _, _), X).
getThird((_, _, X, _, _), X).
getFourth((_, _, _, X, _), X).
getFifth((_, _, _, _, X), X).

% para parametrizar e limpar o código
boxSize(4, 2, 2).
boxSize(6, 3, 2).
boxSize(9, 3, 3).

% Verifica se um número não está repetido na linha atual
isRowValid(Board, Num, Row) :-
    nth0(Row, Board, CurrentRow),
    maplist(firstElem, CurrentRow, RowElements),
    \+ member(Num, RowElements).

% Verifica se um número não está repetido na coluna atual
isColValid(Board, Num, Col) :-
    maplist(nth0(Col), Board, ColElements),
    maplist(firstElem, ColElements, ColNumbers),
    \+ member(Num, ColNumbers).

% Verifica se um número não está repetido na subcaixa atual
isBoxValid(Board, Num, Row, Col) :-
    boxSize(Size, BoxRows, BoxCols),
    StartRow is Row // BoxRows * BoxRows,
    StartCol is Col // BoxCols * BoxCols,
    getBoxElements(Board, StartRow, BoxRows, StartCol, BoxCols, Box),
    maplist(firstElem, Box, BoxNumbers),
    \+ member(Num, BoxNumbers).

getBoxElements(Board, StartRow, BoxRows, StartCol, BoxCols, Box) :-
    findall(Cell, (between(StartRow, StartRow+BoxRows-1, R),
                   between(StartCol, StartCol+BoxCols-1, C),
                   nth0(R, Board, Row),
                   nth0(C, Row, Cell)), Box).

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

% Verifica se um número é válido considerando todas as regras
isValid(Board, Num, Row, Col) :-
    isRowValid(Board, Num, Row),
    isColValid(Board, Num, Col),
    isBoxValid(Board, Num, Row, Col),
    isComparativeValid(Board, Num, Row, Col).

% Substitui um elemento em uma matriz 2D
replace2D(Matrix, (I, J), X, NewMatrix) :-
    nth0(I, Matrix, Row),
    replace(J, Row, X, NewRow),
    replace(I, Matrix, NewRow, NewMatrix).

replace(Index, List, Element, NewList) :-
    nth0(Index, NewList, Element, List).

replace(Index, List, Element, NewList) :-
    nth0(Index, NewList, _, List),
    nth0(Index, List, Element, NewList).

% Encontra uma célula vazia no tabuleiro
find_empty(Board, Row, Col, Row, Col) :-
    nth0(Row, Board, BoardRow),
    nth0(Col, BoardRow, Cell),
    first_elem(Cell, 0).
find_empty(Board, Row, Col, EmptyRow, EmptyCol) :-
    nth0(Row, Board, _),
    length(Board, NumRows),
    Row < NumRows,
    length(BoardRow, NumCols),
    Col < NumCols,
    (Col =:= NumCols-1 ->
        NextRow is Row + 1,
        find_empty(Board, NextRow, 0, EmptyRow, EmptyCol);
        NextCol is Col + 1,
        find_empty(Board, Row, NextCol, EmptyRow, EmptyCol)).

% Tenta preencher uma célula vazia com um número válido
try_number(Board, Num, Row, Col, NewBoard) :-
    Num =< length(Board),
    is_valid(Board, Num, Row, Col),
    replace2D(Board, Row, Col, (Num, _, _, _, _), NewBoard),
    solve_comparative(NewBoard, _).
try_number(Board, Num, Row, Col, NewBoard) :-
    NextNum is Num + 1,
    try_number(Board, NextNum, Row, Col, NewBoard).

% Resolve o tabuleiro
solve_comparative(Board, Solved) :-
    find_empty(Board, 0, 0, Row, Col),
    try_number(Board, 1, Row, Col, Solved).

% Funções de exibição
print_cell((Value, _, _, _, _)) :-
    write(Value),
    write(' ').
print_board([]).
print_board([Row|Rows]) :-
    maplist(print_cell, Row),
    nl,
    print_board(Rows).

% Lê o tabuleiro de uma string
read_board(Input, Board) :-
    read_term_from_atom(Input, Board, []).

% Função principal
main :-
    read_file('../tabuleiro.txt', Content),
    read_board(Content, Board),
    solve_comparative(Board, Solution),
    (   var(Solution) ->
        write('No solution found');
        print_board(Solution)).
