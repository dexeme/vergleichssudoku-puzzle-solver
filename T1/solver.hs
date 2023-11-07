import Data.List (notElem)
import Data.Maybe (fromJust)

-- Definição dos tipos de dados
type Cell = (Int, Char, Char, Char, Char)
type Board = [[Cell]]

-- Funções para obter elementos de uma célula
firstElem (x, _, _, _, _) = x
getSecond (_, x, _, _, _) = x
getThird (_, _, x, _, _) = x
getFourth (_, _, _, x, _) = x
getFifth (_, _, _, _, x) = x

-- para parametrizar e limpar o código
boxSize :: Int -> (Int, Int)
boxSize n
    | n == 4 = (2, 2)
    | n == 6 = (3, 2)
    | n == 9 = (3, 3)
    | otherwise = error "Invalid board size\n"

-- Verifica se um número não está repetido na linha atual
isRowValid :: Board -> Int -> Int -> Bool
isRowValid board num row = notElem num (map firstElem (board !! row))

-- Verifica se um número não está repetido na coluna atual
isColValid :: Board -> Int -> Int -> Bool
isColValid board num col = notElem num [firstElem (board !! r !! col) |
 r <- [0..length board - 1]]

-- Verifica se um número não está repetido na subcaixa atual
isBoxValid :: Board -> Int -> Int -> Int -> Bool
isBoxValid board num row col =
    let (boxRows, boxCols) = boxSize (length board)
        startRow = (row `div` boxRows) * boxRows
        startCol = (col `div` boxCols) * boxCols
        box = [board !! r !! c | r <- [startRow..(startRow
            + boxRows - 1)], c <- [startCol..(startCol 
            + boxCols - 1)]]
    in notElem num (map firstElem box)


-- Verifica se um número é válido considerando as comparações
isComparativeValid :: Board -> Int -> Int -> Int -> Bool
isComparativeValid board num row col =
    let (_, left, up, right, down) = board !! row !! col
        leftVal  = if col == 0 then 0 else firstElem (
            board !! row !! (col-1))
        upVal    = if row == 0 then 0 else firstElem (
            board !! (row-1) !! col)
        rightVal = if col == (length (board !! 0) - 1
            ) then 0 else firstElem (board !! row !! (col+1))
        downVal  = if row == (length board - 1)
             then 0 else firstElem (board !! (row+1) !! col)
        -- Funções para verificar as comparações em todas as direções
        checkLeft
            | col == 0 || leftVal == 0 = True
            | left == '<' = num < leftVal
            | left == '>' = num > leftVal
            | otherwise = True
        checkUp
            | row == 0 || upVal == 0 = True
            | up == '<' = num < upVal
            | up == '>' = num > upVal
            | otherwise = True
        checkRight 
            | col == (length (board !! 0) - 1) || rightVal == 0 = True
            | right == '<' = num < rightVal
            | right == '>' = num > rightVal
            | otherwise = True
        checkDown 
            | row == (length board - 1) || downVal == 0 = True
            | down == '<' = num < downVal
            | down == '>' = num > downVal
            | otherwise = True

    in checkLeft && checkUp && checkRight && checkDown

-- Verifica se um número é válido considerando todas as regras
isValid :: Board -> Int -> Int -> Int -> Bool
isValid board num row col =
    isRowValid board num row && isColValid board num col &&
    isBoxValid board num row col && isComparativeValid board num row col

-- Substitui um elemento em uma lista 2D
replace2D :: Board -> (Int, Int) -> Cell -> Board
replace2D matrix (i, j) x = 
    take i matrix ++ [replace (matrix !! i) j x] ++ drop (i+1) matrix
  where
    replace row j x = take j row ++ [x] ++ drop (j+1) row

-- Encontra uma célula vazia no tabuleiro
findEmpty :: Board -> Int -> Int -> Maybe (Int, Int)
findEmpty board row col
    | row == length board = Nothing
    | firstElem (board !! row !! col) == 0 = Just (row, col)
    | col == (length (board !! 0) - 1) = findEmpty board (row+1) 0
    | otherwise = findEmpty board row (col+1)

-- Tenta preencher uma célula vazia com um número válido
tryNumber :: Board -> Int -> Int -> Int -> Maybe Board
tryNumber board num row col
    | num > length board = Nothing
    | isValid board num row col = 
        let newCell = (num, getSecond (board !! row !! col),
                            getThird (board !! row !! col),
                            getFourth (board !! row !! col),
                            getFifth (board !! row !! col))
            newBoard = replace2D board (row, col) newCell
            nextAttempt = solveComparative newBoard
        in if nextAttempt == Nothing
           then tryNumber board (num+1) row col
           else nextAttempt
    | otherwise = tryNumber board (num+1) row col

-- Resolve o tabuleiro
solveComparative :: Board -> Maybe Board
solveComparative board
    | findEmpty board 0 0 == Nothing = Just board
    | otherwise = 
        let (row, col) = fromJust (findEmpty board 0 0)
        in tryNumber board 1 row col

-- Funções de exibição
printCell :: Cell -> IO ()
printCell (value, _, _, _, _) = putStr (show value ++ " ")

printBoard :: Board -> IO ()
printBoard [] = return ()
printBoard (row:rows) = do
    mapM_ printCell row
    putStrLn ""
    printBoard rows

-- Lê o tabuleiro de uma string
readBoard :: String -> Board
readBoard input = read input :: Board

-- Função principal
main :: IO ()
main = do
    content <- readFile "../tabuleiro.txt"
    let board = readBoard content
    let solution = solveComparative board
    case solution of
         Just solved -> printBoard solved
         Nothing -> putStrLn "No solution found"
