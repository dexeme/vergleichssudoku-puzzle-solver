:- use_module(library(clpfd)).
:- use_module(library(readutil)).

% Lê o tabuleiro de um arquivo
read_board_from_file(FileName, Board) :-
    read_file_to_string(FileName, String, []),
    term_string(Board, String).

% Imprimir o tabuleiro de forma legível
print_board(Board) :-
    nl,
    maplist(print_row, Board),
    nl.

print_row(Row) :-
    maplist(print_cell, Row),
    nl.

print_cell([Value, _]) :-
    write(Value),
    write(' ').

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

% para comparar apenas o valor da célula
extract_values(Board, Extracted) :-
    maplist(extract_row, Board, Extracted).

extract_row(Row, Extracted) :-
    maplist(extract_cell, Row, Extracted).

extract_cell([CellValue, _], Value) :-
   CellValue #= Value.

% Função para inicializar as células com domínios possíveis
initialize_cells(Board, Size) :-
    maplist(initialize_row(Size), Board).

initialize_row(Size, Row) :-
    maplist(initialize_cell(Size), Row).

initialize_cell(Size, [Cell, [Left, Up, Right, Down]]) :-
    apply_comparative_restriction(Cell, Left, Size),
    apply_comparative_restriction(Cell, Up, Size),
    apply_comparative_restriction(Cell, Right, Size),
    apply_comparative_restriction(Cell, Down, Size),
    [Cell] ins 1..Size.

apply_comparative_restriction(Cell, Comp, Size) :-
    (Comp == '/');
    (Comp == '>', Cell #> 1);
    (Comp == '<', Cell #< Size).

% Valida as comparações entre células
isComparativeValid(Board) :-
    length(Board, Size),
    numlist(1, Size, Indices),
    maplist(validate_row_comparisons(Board), Indices, Board).

% Valida as comparações em uma linha
validate_row_comparisons(Board, Y, Row) :-
    length(Row, Length),
    numlist(1, Length, Xs),
    maplist(validate_cell_comparisons(Board, Y), Xs, Row).

% Valida comparações de uma célula com suas adjacentes
validate_cell_comparisons(Board, Y, X, [CellValue, [Left, Up, Right, Down]]) :-
    adjacent_cell(Board, X, Y, -1, 0, LeftValue), % Célula à esquerda
    adjacent_cell(Board, X, Y, 1, 0, RightValue), % Célula à direita
    adjacent_cell(Board, X, Y, 0, -1, UpValue),   % Célula acima
    adjacent_cell(Board, X, Y, 0, 1, DownValue),  % Célula abaixo
    apply_comparison(CellValue, LeftValue, Left),
    apply_comparison(CellValue, UpValue, Up),
    apply_comparison(CellValue, RightValue, Right),
    apply_comparison(CellValue, DownValue, Down).

% Obtem o valor de uma célula adjacente
adjacent_cell(Board, X, Y, DX, DY, AdjValue) :-
    NewX is X + DX,
    NewY is Y + DY,
    length(Board, MaxSize),
    ((NewX > 0, NewY > 0, NewX =< MaxSize, NewY =< MaxSize) ->
        nth1(NewY, Board, Row),
        nth1(NewX, Row, [AdjValue, _]);
    AdjValue = '/').

% Aplica a comparação se necessário
apply_comparison(CellValue, AdjValue, Comp) :-
    (Comp == '/', true); % Ignora comparações com células fora do tabuleiro
    (Comp == '>', CellValue #> AdjValue);
    (Comp == '<', CellValue #< AdjValue).

% Função para resolver o Sudoku
solve(Board) :-
    length(Board, Size),
    initialize_cells(Board, Size),
    isComparativeValid(Board),
    extract_values(Board, ValuesBoard),
    isRowValid(ValuesBoard),
    isColValid(ValuesBoard),
    isBoxValid(Size, ValuesBoard).

% Função principal
:- initialization(main).
main :-
    read_board_from_file('../tabuleiro.txt', Board),
    (   solve(Board) -> 
        print_board(Board);
        writeln('No solution found')
    ).
