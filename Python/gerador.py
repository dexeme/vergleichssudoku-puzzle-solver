import ast


# Função para formatar o tabuleiro
# de acordo com as regras fornecidas
def format_sudoku(board):
    formatted_board = []
    size = len(board)

    for i in range(size):
        row = []
        for j in range(size):
            sinais_de_maior = 0
            sinais_de_menor = 0
            # Para a esquerda
            if j == 0:
                left = '/'
            elif board[i][j] > board[i][j-1]:
                left = '>'
                sinais_de_maior += 1
            else:
                left = '<'
                sinais_de_menor += 1

            # Para cima
            if i == 0:
                up = '/'
            elif board[i][j] > board[i-1][j]:
                up = '>'
                sinais_de_maior += 1
            else:
                up = '<'
                sinais_de_menor += 1

            # Para a direita
            if j == size-1:
                right = '/'
            elif board[i][j] > board[i][j+1]:
                right = '>'
                sinais_de_maior += 1
            else:
                right = '<'
                sinais_de_menor += 1

            # Para baixo
            if i == size-1:
                down = '/'
            elif board[i][j] > board[i+1][j]:
                down = '>'
                sinais_de_maior += 1
            else:
                down = '<'
                sinais_de_menor += 1

            # Adicionando ao formato da célula
            # numeros_disponiveis = [
            # i for i in range(1 + sinais_de_maior, 10 - sinais_de_menor)]
            cell_format = f"{left} {up} {right} {down}"
            # print(cell_format)
            row.append(cell_format)
        formatted_board.append(row)
    return formatted_board


def reformat_comparative_sudoku(board):
    reformatted_board = []

    for row in board:
        reformatted_row = []
        for cell in row:
            left, up, right, down = cell.split()
            reformatted_cell = [0, left, up, right, down]
            reformatted_row.append(reformatted_cell)
        reformatted_board.append(reformatted_row)

    return reformatted_board


def reformat_comparative_sudoku_for_hs(board):
    reformatted_board = []

    for row in board:
        reformatted_row = []
        for cell in row:
            left, up, right, down = cell.split()
            reformatted_cell = (0, left, up, right, down)
            reformatted_row.append(reformatted_cell)
        reformatted_board.append(reformatted_row)

    return reformatted_board


def reformat_comparative_sudoku_for_lisp(board):
    reformatted_board = []
    for row in board:
        reformatted_row = []
        for cell in row:
            reformatted_cell = "(0 " + cell + ')'
            reformatted_row.append(reformatted_cell)
        reformatted_board.append(tuple(reformatted_row))

    return tuple(reformatted_board)


def print_board(board):
    size = len(board)
    # Check box
    if size == 4:
        box_rows, box_cols = 2, 2
    elif size == 6:
        box_rows, box_cols = 3, 2
    else:
        box_rows, box_cols = 3, 3
    for row in range(size):
        if row % box_rows == 0 and row != 0:
            print("-" * ((size + 2) * 2))
        for col in range(size):
            if col % box_cols == 0 and col != 0:
                print("| ", end="")
            if col == size - 1:
                print(board[row][col][0])
            else:
                print(board[row][col][0], end=" ")


def print_formatted_board(board):
    size = len(board)
    # Check box
    if size == 4:
        box_rows, box_cols = 2, 2
    elif size == 6:
        box_rows, box_cols = 3, 2
    else:
        box_rows, box_cols = 3, 3
    for row in range(size):
        if row % box_rows == 0 and row != 0:
            print("-" * ((size + 2) * 2))
        for col in range(size):
            if col % box_cols == 0 and col != 0:
                print("| ", end="")
            if col == size - 1:
                print(board[row][col])
            else:
                print(board[row][col], end=" ; ")


# Gerando um tabuleiro de Sudoku resolvido
# (um padrão fixo para simplicidade)

def write_solved_board_on_file(filename, board):
    with open(filename, 'w') as f:
        for row in board:
            f.write(str(row) + "\n")


def read_solved_board_from_file(filename):
    with open(filename, 'r') as f:
        lines = f.readlines()
        # Convertendo cada linha (string) de volta para uma lista
        board = [ast.literal_eval(line.strip()) for line in lines]
    return board


def write_lisp_board_on_file(f, board):
    tabuleiro = ["("]
    for row in board:
        tabuleiro.append("(")
        for cell in row:
            tabuleiro.append(cell)
            tabuleiro.append(' ')
        tabuleiro.pop()
        tabuleiro.append(")")
    tabuleiro.append(")")
    tabuleiro = ''.join(tabuleiro)
    f.write(tabuleiro)


def reformat_comparative_sudoku_for_prolog(board):
    reformatted_board = []
    row_number = 0
    for row in board:
        row_number += 1
        reformatted_row = []
        col_number = 0
        for cell in row:
            col_number += 1
            value = "_"
            reformatted_cell = [value, cell.split()]
            reformatted_row.append(reformatted_cell)
        reformatted_board.append(reformatted_row)

    return reformatted_board


def write_prolog_board_on_file(f, board):
    tabuleiro = ["["]
    for row in board:
        tabuleiro.append("[")
        for cell in row:
            value, comparatives = cell
            tabuleiro.append("[")
            tabuleiro.append(value)
            tabuleiro.append(', ')
            tabuleiro.append(str(comparatives))
            tabuleiro.append("]")
            tabuleiro.append(', ')
        tabuleiro.pop()
        tabuleiro.append("],\n")
    tabuleiro.pop()
    tabuleiro.append("]].")
    tabuleiro = ''.join(tabuleiro)
    f.write(tabuleiro)


sudoku1 = [
    [5, 3, 4, 6, 7, 8, 9, 1, 2],
    [6, 7, 2, 1, 9, 5, 3, 4, 8],
    [1, 9, 8, 3, 4, 2, 5, 6, 7],
    [8, 5, 9, 7, 6, 1, 4, 2, 3],
    [4, 2, 6, 8, 5, 3, 7, 9, 1],
    [7, 1, 3, 9, 2, 4, 8, 5, 6],
    [9, 6, 1, 5, 3, 7, 2, 8, 4],
    [2, 8, 7, 4, 1, 9, 6, 3, 5],
    [3, 4, 5, 2, 8, 6, 1, 7, 9]
]

sudoku2 = [
    [8, 2, 7, 1, 5, 4, 3, 9, 6],
    [9, 6, 5, 3, 2, 7, 1, 4, 8],
    [3, 4, 1, 6, 8, 9, 7, 5, 2],
    [5, 9, 3, 4, 6, 8, 2, 7, 1],
    [4, 7, 2, 5, 1, 3, 6, 8, 9],
    [6, 1, 8, 9, 7, 2, 4, 3, 5],
    [7, 8, 6, 2, 3, 5, 9, 1, 4],
    [1, 5, 4, 7, 9, 6, 8, 2, 3],
    [2, 3, 9, 8, 4, 1, 5, 6, 7]
]

sudoku6x6 = [
    [4, 6, 5, 1, 3, 2],
    [5, 3, 6, 2, 4, 1],
    [1, 2, 3, 4, 5, 6],
    [3, 4, 2, 6, 1, 5],
    [6, 5, 1, 3, 2, 4],
    [2, 1, 4, 5, 6, 3]
]

sudoku4x4 = [
    [2, 1, 4, 3],
    [4, 3, 2, 1],
    [1, 2, 3, 4],
    [3, 4, 1, 2]
]


def decide_sudoku():
    print("Escolha o sudoku: 1-9x9, 2-9x9, 3-6x6, 4-4x4")
    esc = input()
    if esc == "1":
        return sudoku1
    elif esc == "2":
        return sudoku2
    elif esc == "3":
        return sudoku6x6
    elif esc == "4":
        return sudoku4x4
    else:
        return sudoku1


def main():
    linguagem = input("selecione a linguagem: 1-Haskell 2-Lisp 3-Prolog\n")
    board = decide_sudoku()
    formatted_sudoku = format_sudoku(board)
    write_solved_board_on_file("../Python/tabuleiros_prontos.txt", board)
    with open("../tabuleiro.txt", 'w') as arquivo:
        if linguagem == "1":
            board = reformat_comparative_sudoku_for_hs(
                formatted_sudoku)
            arquivo.write(str(board))
        elif linguagem == "2":
            board = reformat_comparative_sudoku_for_lisp(formatted_sudoku)
            write_lisp_board_on_file(arquivo, board)
        elif linguagem == "3":
            board = reformat_comparative_sudoku_for_prolog(formatted_sudoku)
            write_prolog_board_on_file(arquivo, board)
        else:
            print("linguagem não reconhecida")


main()
