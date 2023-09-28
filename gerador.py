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
            if j == 8:
                right = '/'
            elif board[i][j] > board[i][j+1]:
                right = '>'
                sinais_de_maior += 1
            else:
                right = '<'
                sinais_de_menor += 1

            # Para baixo
            if i == 8:
                down = '/'
            elif board[i][j] > board[i+1][j]:
                down = '>'
                sinais_de_maior += 1
            else:
                down = '<'
                sinais_de_menor += 1

            # Adicionando ao formato da célula
            numeros_disponiveis = [i for i in range(1 + sinais_de_maior, 10 - sinais_de_menor)]
            cell_format = f"{left} {up} {right} {down} {numeros_disponiveis}"
            print(cell_format)
            row.append(cell_format)

        formatted_board.append(row)
    return formatted_board


# Formatando o tabuleiro resolvido
formatted_sudoku = format_sudoku(sudoku)

