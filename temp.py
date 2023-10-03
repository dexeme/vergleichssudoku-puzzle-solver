import gerador


def is_valid(board, num, row, col):
    size = len(board)

    # Check row
    for i in range(size):
        if board[row][i][0] == num:
            return False

    # Check column
    for i in range(size):
        if board[i][col][0] == num:
            return False

    # Check box
    if size == 9:
        box_rows, box_cols = 3, 3
    elif size == 4:
        box_rows, box_cols = 2, 2
    else:
        box_rows, box_cols = 3, 2

    box_x = col // box_cols
    box_y = row // box_rows
    for i in range(box_y * box_rows, (box_y + 1) * box_rows):
        for j in range(box_x * box_cols, (box_x + 1) * box_cols):
            if board[i][j][0] == num:
                return False

    # Check the comparative rules
    # Left comparison
    if col - 1 >= 0:
        if board[row][col][1] == '<' and num <= board[row][col-1][0]:
            return False
        if board[row][col][1] == '>' and num >= board[row][col-1][0]:
            return False

    # Up comparison
    if row - 1 >= 0:
        if board[row][col][2] == '<' and num <= board[row-1][col][0]:
            return False
        if board[row][col][2] == '>' and num >= board[row-1][col][0]:
            return False

    # Right comparison
    if col + 1 < size:
        if board[row][col][3] == '<' and num <= board[row][col+1][0]:
            return False
        if board[row][col][3] == '>' and num >= board[row][col+1][0]:
            return False

    # Down comparison
    if row + 1 < size:
        if board[row][col][4] == '<' and num <= board[row+1][col][0]:
            return False
        if board[row][col][4] == '>' and num >= board[row+1][col][0]:
            return False

    return True


def find_empty(board):
    for row in range(len(board)):
        for col in range(len(board[0])):
            if board[row][col][0] == 0:
                return (row, col)
    return None


def solve_comparative(board):
    find = find_empty(board)
    if not find:
        return True
    else:
        row, col = find

    for num in range(1, len(board) + 1):
        if is_valid(board, num, row, col):
            board[row][col][0] = num

            if solve_comparative(board):
                return True

            board[row][col][0] = 0

    return False


# Teste

comparative_sudoku_board = gerador.comparative_sudoku_board
solve_comparative(comparative_sudoku_board)
