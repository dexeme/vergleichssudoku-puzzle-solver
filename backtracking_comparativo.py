# import gerador


def find_empty(board):
    for row in range(len(board)):
        for col in range(len(board[0])):
            if board[row][col][0] == 0:
                return (row, col)
    return None


def valid(board, num, row, col):
    size = len(board)

    # Verificar linha
    row_valid = all([board[i][0] != num for i in range(size)])
    # Verificar coluna
    col_valid = all([board[row][i][0] != num for i in range(size)])

    # Determine box size and validate box
    if size == 4:
        box_rows, box_cols = 2, 2
    elif size == 6:
        box_rows, box_cols = 3, 2
    else:
        box_rows, box_cols = 3, 3

    box_x = col // box_cols
    box_y = row // box_rows
    box_valid = all([board[i][j][0] != num
                     for i in range(box_y * box_rows, (box_y + 1) * box_rows)
                     for j in range(box_x * box_cols, (box_x + 1) * box_cols)])

    # Check comparisons
    cell = board[row][col]
    comp_valid = True

    # Check right comparison
    if col + 1 < size and cell[1] != '/':
        if cell[1] == '<' and num >= board[row][col+1][0]:
            comp_valid = False
        elif cell[1] == '>' and num <= board[row][col+1][0]:
            comp_valid = False

    # Check up comparison
    if row - 1 >= 0 and cell[2] != '/':
        if cell[2] == '<' and num >= board[row-1][col][0]:
            comp_valid = False
        elif cell[2] == '>' and num <= board[row-1][col][0]:
            comp_valid = False

    # Check left comparison
    if col - 1 >= 0 and cell[3] != '/':
        if cell[3] == '<' and num >= board[row][col-1][0]:
            comp_valid = False
        elif cell[3] == '>' and num <= board[row][col-1][0]:
            comp_valid = False

    # Check down comparison
    if row + 1 < size and cell[4] != '/':
        if cell[4] == '<' and num >= board[row+1][col][0]:
            comp_valid = False
        elif cell[4] == '>' and num <= board[row+1][col][0]:
            comp_valid = False

    return row_valid and col_valid and box_valid and comp_valid


def solve(board):
    empty = find_empty(board)
    if not empty:
        return board

    row, col = empty
    size = len(board)

    for num in range(1, size + 1):
        if valid(board, num, row, col):
            board[row][col][0] = num
            if solve(board):
                return board
            board[row][col][0] = 0
    return None


def print_board(board):
    size = len(board)
    for row in range(size):
        if row % 3 == 0 and row != 0:
            print("-" * (size * 3))
        for col in range(size):
            if col % 3 == 0 and col != 0:
                print("| ", end="")
            if col == size - 1:
                print(board[row][col][0])
            else:
                print(board[row][col][0], end=" ")


# Test with a 9x9 board
board_9x9 = [[[
  [0, '/', '/', '>', '>'],
  [0, '<', '/', '<', '<'],
  [0, '>', '/', '>', '>'],
  [0, '<', '/', '>', '<'],
  [0, '<', '/', '<', '<'],
  [0, '>', '/', '<', '<'],
  [0, '>', '/', '>', '>'],
  [0, '<', '/', '<', '>'],
  [0, '>', '/', '/', '>']],
 [[0, '/', '<', '>', '>'],
  [0, '<', '>', '>', '<'],
  [0, '<', '<', '<', '<'],
  [0, '>', '>', '<', '>'],
  [0, '>', '>', '<', '>'],
  [0, '>', '>', '>', '>'],
  [0, '<', '<', '<', '<'],
  [0, '>', '<', '<', '>'],
  [0, '>', '<', '/', '>']],
 [[0, '/', '<', '<', '<'],
  [0, '>', '>', '>', '>'],
  [0, '<', '>', '>', '>'],
  [0, '<', '<', '<', '<'],
  [0, '>', '<', '>', '>'],
  [0, '<', '<', '>', '<'],
  [0, '<', '>', '>', '<'],
  [0, '<', '<', '>', '<'],
  [0, '<', '<', '/', '<']],
 [[0, '/', '>', '>', '>'],
  [0, '<', '<', '<', '<'],
  [0, '>', '<', '<', '>'],
  [0, '>', '>', '>', '<'],
  [0, '<', '<', '<', '<'],
  [0, '>', '>', '>', '>'],
  [0, '<', '>', '>', '<'],
  [0, '<', '>', '>', '>'],
  [0, '<', '>', '/', '<']],
 [[0, '/', '<', '<', '<'],
  [0, '>', '>', '>', '>'],
  [0, '<', '<', '<', '<'],
  [0, '>', '>', '>', '>'],
  [0, '<', '>', '>', '>'],
  [0, '<', '<', '<', '>'],
  [0, '>', '>', '>', '>'],
  [0, '<', '<', '<', '<'],
  [0, '>', '>', '/', '<']],
 [[0, '/', '>', '>', '>'],
  [0, '<', '<', '<', '>'],
  [0, '>', '>', '>', '<'],
  [0, '<', '<', '<', '<'],
  [0, '>', '<', '>', '<'],
  [0, '<', '<', '<', '>'],
  [0, '>', '<', '>', '>'],
  [0, '<', '>', '<', '<'],
  [0, '>', '>', '/', '>']],
 [[0, '/', '<', '<', '<'],
  [0, '>', '<', '<', '<'],
  [0, '>', '>', '<', '>'],
  [0, '>', '>', '>', '>'],
  [0, '<', '>', '>', '<'],
  [0, '<', '<', '<', '>'],
  [0, '>', '<', '<', '<'],
  [0, '>', '>', '>', '>'],
  [0, '<', '<', '/', '>']],
 [[0, '/', '>', '<', '<'],
  [0, '>', '>', '>', '>'],
  [0, '<', '<', '<', '<'],
  [0, '>', '<', '<', '>'],
  [0, '>', '>', '>', '>'],
  [0, '<', '<', '<', '<'],
  [0, '>', '>', '<', '>'],
  [0, '>', '<', '>', '<'],
  [0, '<', '<', '/', '>']],
 [[0, '/', '>', '<', '/'],
  [0, '>', '<', '<', '/'],
  [0, '>', '>', '>', '/'],
  [0, '<', '<', '>', '/'],
  [0, '<', '<', '<', '/'],
  [0, '>', '>', '>', '/'],
  [0, '<', '<', '<', '/'],
  [0, '>', '>', '>', '/'],
  [0, '<', '<', '/', '/']]]
]

print("Original 9x9 Sudoku:")
print_board(board_9x9)
result_9x9 = solve(board_9x9)
print("-------------------\n")
print("Solved 9x9 Sudoku:")
if result_9x9:
    print_board(result_9x9)
else:
    print("No solution exists for the provided 9x9 Sudoku.")
