def find_empty(board, row=0, col=0):
    # Se ultrapassarmos as linhas, não encontramos uma célula vazia.
    if row == len(board):
        return None
    # Se encontrarmos uma célula vazia, retornamos sua posição.
    if board[row][col] == 0:
        return (row, col)

    # Avançar para a próxima célula.
    col += 1
    if col == len(board[row]):
        col = 0
        row += 1
    # Chamada recursiva para a próxima célula.
    return find_empty(board, row, col)


def valid(board, num, row, col):
    size = len(board)
    # Determinar o tamanho das regiões com base no tamanho do tabuleiro.
    box_rows, box_cols = (2, 3) if size == 6 else (
        int(size**0.5), int(size**0.5))

    # Verificar se o número é válido na linha.
    row_valid = all([board[row][i] != num for i in range(size)])
    # Verificar se o número é válido na coluna.
    col_valid = all([board[i][col] != num for i in range(size)])

    # Identificar a caixa/região à qual a célula pertence.
    box_x = col // box_cols
    box_y = row // box_rows
    # Verificar se o número é válido na caixa/região.
    box_valid = all([board[i][j] != num
                     for i in range(box_y * box_rows,
                                    (box_y + 1) * box_rows)
                     for j in range(box_x * box_cols,
                                    (box_x + 1) * box_cols)])

    # Retorna True se o número for válido em todas as verificações.
    return row_valid and col_valid and box_valid


def solve_cell(board, num, row, col):
    # Verificar se o número é válido para a célula.
    if not valid(board, num, row, col):
        return None
    # Se for válido, criar uma cópia do
    # tabuleiro e colocar o número na célula.
    new_board = [list(r) for r in board]
    new_board[row][col] = num
    # Tentar resolver o tabuleiro com o novo número.
    return solve(new_board)


def solve(board):
    # Procurar uma célula vazia.
    empty = find_empty(board)
    # Se não houver célula vazia,
    # o tabuleiro está completo.
    if not empty:
        return board
    row, col = empty
    size = len(board)
    # Tentar inserir números de 1
    # ao tamanho do tabuleiro na
    # célula vazia.
    for num in range(1, size + 1):
        # Se um número for válido,
        # continuamos resolvendo a
        # partir dessa configuração.
        solved = solve_cell(board, num, row, col)
        if solved:
            return solved
    # Se nenhum número for válido,
    # retornamos None para indicar
    # uma solução inválida.
    return None


def print_row(row, col_idx=0):
    size = len(row)
    # Determinar o tamanho das regiões com base no tamanho do tabuleiro.
    box_size = 2 if size == 6 else int(size**0.5)
    if col_idx == size:
        print()
        return
    sep = " | " if col_idx % box_size == box_size - 1 else " "
    print(row[col_idx], end=sep)
    print_row(row, col_idx + 1)


def print_board(bo, row_idx=0):
    if not bo:
        print("No solution exists for the provided Sudoku.")
        return
    size = len(bo)
    # Determinar o tamanho das regiões com base no tamanho do tabuleiro.
    box_size = 2 if size == 6 else int(size**0.5)
    if row_idx == size:
        return
    if row_idx % box_size == 0 and row_idx != 0:
        print("-" * (2 * size + box_size - 1))
    print_row(bo[row_idx])
    print_board(bo, row_idx + 1)


# Teste 4x4
board_4x4 = [
    [0, 2, 0, 0],
    [4, 0, 3, 0],
    [0, 3, 0, 4],
    [0, 0, 2, 0]
]

# Teste 6x6
board_6x6 = [
    [5, 0, 0, 6, 0, 4],
    [0, 0, 6, 0, 0, 0],
    [0, 4, 0, 0, 5, 0],
    [0, 2, 0, 0, 3, 0],
    [0, 0, 0, 4, 0, 0],
    [1, 0, 5, 0, 0, 6]
]
# Teste 9x9
board_9x9 = [
    [7, 8, 0, 4, 0, 0, 1, 2, 0],
    [6, 0, 0, 0, 7, 5, 0, 0, 9],
    [0, 0, 0, 6, 0, 1, 0, 7, 8],
    [0, 0, 7, 0, 4, 0, 2, 6, 0],
    [0, 0, 1, 0, 5, 0, 9, 3, 0],
    [9, 0, 4, 0, 6, 0, 0, 0, 5],
    [0, 7, 0, 3, 0, 0, 0, 1, 2],
    [1, 2, 0, 0, 0, 7, 4, 0, 0],
    [0, 4, 9, 2, 0, 6, 0, 0, 7]
]

print("4--------------------------------4\n")
print_board(board_4x4)
result = solve(board_4x4)
print("__________________________\n")
print_board(result)
'''
print("\n6--------------------------------6\n")
print_board(board_6x6)
result = solve(board_6x6)
print("__________________________\n")
print_board(result) '''


print("\n9--------------------------------9\n")
print_board(board_9x9)
result = solve(board_9x9)
print("__________________________\n")
print_board(result)
