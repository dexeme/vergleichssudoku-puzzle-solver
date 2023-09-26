import file

SIZE = 9

board = file.file_to_board()

formatted_board = file.format_board(board)

def read_file():
    with open('tabuleiros_formatados.txt') as f:
        lines = f.readlines()
        boards = []
        linha = []
        print('')
        row = 0
        for line in lines:
            row += 1
            ranges = [1, SIZE + 1]
            for char in line:
                if char == ">":
                    ranges[0] += 1
                elif char == "<":
                    ranges[1] -= 1 
            celula = [i for i in range(ranges[0], ranges[1])]
            linha.append(celula)
            if row == SIZE:
                boards.append(linha)
                row = 0
                linha = []
            print(celula)
        return boards

board = read_file()

''''
for j in range(3):
    l = (i//3)*3 + j
    for k in range(3):
        c = (i % 3)*3 + k
'''

# celulas_prontas = 0
# while celulas_prontas < SIZE**2:
#     for row in range(SIZE):
#         for col in range(SIZE):
#             lim_lin, lim_col = 3, 3
#             if SIZE == 4:
#                 lim_col, lim_lin = 2, 2
#             elif SIZE == 6:
#                 lim_col, lim_lin = 3, 2
#             for l in range(lim_lin):
#                 for c in range(lim_col):
                    