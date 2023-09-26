import numpy as np

# Gerando um tabuleiro de Sudoku resolvido (um padrão fixo para simplicidade)
sudoku = np.array([
    [5, 3, 4, 6, 7, 8, 9, 1, 2],
    [6, 7, 2, 1, 9, 5, 3, 4, 8],
    [1, 9, 8, 3, 4, 2, 5, 6, 7],
    [8, 5, 9, 7, 6, 1, 4, 2, 3],
    [4, 2, 6, 8, 5, 3, 7, 9, 1],
    [7, 1, 3, 9, 2, 4, 8, 5, 6],
    [9, 6, 1, 5, 3, 7, 2, 8, 4],
    [2, 8, 7, 4, 1, 9, 6, 3, 5],
    [3, 4, 5, 2, 8, 6, 1, 7, 9]
])


# Função para formatar o tabuleiro de acordo com as regras fornecidas
def format_sudoku(board):
    formatted_board = []

    for i in range(9):
        row = []
        for j in range(9):
            # Para a esquerda
            left = '/' if j == 0 else '>' if board[i][j] > board[i][j-1] else '<'

            # Para cima
            up = '/' if i == 0 else '>' if board[i][j] > board[i-1][j] else '<'

            # Para a direita
            right = '/' if j == 8 else '>' if board[i][j] > board[i][j+1] else '<'

            # Para baixo
            down = '/' if i == 8 else '>' if board[i][j] > board[i+1][j] else '<'

            # Adicionando ao formato da célula
            cell_format = f"{left} {up} {right} {down}"
            row.append(cell_format)

        formatted_board.append(row)
    return formatted_board


# Formatando o tabuleiro resolvido
formatted_sudoku = format_sudoku(sudoku)
formatted_sudoku
