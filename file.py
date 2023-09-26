# Lê o arquivo de entrada "tabuleiros.txt" e retorna um tabuleiro

def file_to_board():
    # Lê o arquivo de entrada e retorna uma lista de tabuleiros
    with open('tabuleiros.txt', 'r') as f:
        lines = f.readlines()
        boards = []
        for line in lines:
            board = []
            for char in line:
                if char != '\n' and char != ' ':
                    board.append(int(char))
            boards.append(board)
            print(board)
        return boards

# Com o tabuleiro em mãos, passamos por cada célula atribuindo um sinal de maior ou menor para cada célula adjacente

# Função para formatar o tabuleiro de acordo com as regras fornecidas
def format_board(board):
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

def print_format_board(formated_board):
    # print each cell
    for row in formated_board:
        for cell in row:
            print(cell)
