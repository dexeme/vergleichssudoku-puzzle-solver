import gerador


def find_empty(board, row=0, col=0):
    # Se ultrapassarmos as linhas, não encontramos uma célula vazia.
    if row == len(board):
        return None
    # Se encontrarmos uma célula vazia, retornamos sua posição.
    if board[row][col][0] == 0:
        return (row, col)

    # Avançar para a próxima célula.
    col += 1
    if col == len(board[row]):
        col = 0
        row += 1
    # Chamada recursiva para a próxima célula.
    return find_empty(board, row, col)


def is_valid(board, num, row, col):
    size = len(board)

    # Verificar se o número é válido na linha.
    row_valid = all([board[row][i][0] != num for i in range(size)])
    # Verificar se o número é válido na coluna.
    col_valid = all([board[i][col][0] != num for i in range(size)])

    # Check box
    if size == 4:
        box_rows, box_cols = 2, 2
    elif size == 6:
        box_rows, box_cols = 3, 2
    else:
        box_rows, box_cols = 3, 3

    # Identificar a caixa/região à qual a célula pertence.
    box_x = col // box_cols
    box_y = row // box_rows
    # Verificar se o número é válido na caixa/região.
    box_valid = all([board[i][j][0] != num
                     for i in range(box_y * box_rows,
                                    (box_y + 1) * box_rows)
                     for j in range(box_x * box_cols,
                                    (box_x + 1) * box_cols)])

    if not (row_valid and col_valid and box_valid):
        return False

    # Check the comparative rules
    # Left comparison
    if col > 0 and board[row][col-1][0] > 0:
        if board[row][col][1] == '>' and num < board[row][col-1][0]:
            return False
        if board[row][col][1] == '<' and num > board[row][col-1][0]:
            return False

    # Up comparison
    if row > 0 and board[row-1][col][0] > 0:
        if board[row][col][2] == '>' and num < board[row-1][col][0]:
            return False
        if board[row][col][2] == '<' and num > board[row-1][col][0]:
            return False

    # Right comparison
    if col + 1 < size and board[row][col+1][0] > 0:
        if board[row][col][3] == '>' and num < board[row][col+1][0]:
            return False
        if board[row][col][3] == '<' and num > board[row][col+1][0]:
            return False

    # Down comparison
    if row + 1 < size and board[row+1][col][0] > 0:
        if board[row][col][4] == '>' and num < board[row+1][col][0]:
            return False
        if board[row][col][4] == '<' and num > board[row+1][col][0]:
            return False

    return True


def try_number(board, num, row, col):
    if num > len(board):
        return False

    if is_valid(board, num, row, col):
        board[row][col][0] = num
        if solve_comparative(board):
            return True
        board[row][col][0] = 0

    return try_number(board, num + 1, row, col)


def solve_comparative(board):
    find = find_empty(board)
    if not find:
        return True
    row, col = find
    return try_number(board, 1, row, col)


def main():
    formatted_sudoku = gerador.format_sudoku(gerador.sudoku4x4)
    comparative_sudoku_board = gerador.reformat_comparative_sudoku(
        formatted_sudoku)
    print("--------inicial----------\n")
    gerador.print_board(comparative_sudoku_board)
    print("\n--------resolucao---------\n")
    solve_comparative(comparative_sudoku_board)
    gerador.print_board(comparative_sudoku_board)


main()
