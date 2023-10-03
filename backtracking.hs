import Data.List (notElem)
import Data.Maybe (fromJust)

type Cell = (Int, Char, Char, Char, Char)
type Board = [[Cell]]

-- Verifica se um número é válido na linha atual
isRowValid :: Board -> Int -> Int -> Bool
isRowValid board num row = notElem num (map fst (board !! row))

-- Verifica se um número é válido na coluna atual
isColValid :: Board -> Int -> Int -> Bool
isColValid board num col = notElem num [fst (board !! r !! col) | r <- [0..length board - 1]]

-- Verifica se um número é válido na subcaixa atual
isBoxValid :: Board -> Int -> Int -> Int -> Bool
isBoxValid board num row col =
    let boxRows = if length board == 4 then 2 else if length board == 6 then 3 else 3
        boxCols = if length board == 4 then 2 else if length board == 6 then 2 else 3
        startRow = (row `div` boxRows) * boxRows
        startCol = (col `div` boxCols) * boxCols
        box = [board !! r !! c | r <- [startRow..(startRow + boxRows - 1)], c <- [startCol..(startCol + boxCols - 1)]]
    in notElem num (map fst box)

-- Verifica se um número é válido considerando as comparações
isComparativeValid :: Board -> Int -> Int -> Int -> Bool
isComparativeValid board num row col =
    let (_, left, up, right, down) = board !! row !! col
        leftVal  = if col == 0 then 0 else fst (board !! row !! (col-1))
        upVal    = if row == 0 then 0 else fst (board !! (row-1) !! col)
        rightVal = if col == (length (board !! 0) - 1) then 0 else fst (board !! row !! (col+1))
        downVal  = if row == (length board - 1) then 0 else fst (board !! (row+1) !! col)
    in (left == '/' || (left == '<' && num < leftVal) || (left == '>' && num > leftVal)) &&
       (up == '/' || (up == '<' && num < upVal) || (up == '>' && num > upVal)) &&
       (right == '/' || (right == '<' && num < rightVal) || (right == '>' && num > rightVal)) &&
       (down == '/' || (down == '<' && num < downVal) || (down == '>' && num > downVal))

-- Verifica se um número é válido considerando todas as regras
isValid :: Board -> Int -> Int -> Int -> Bool
isValid board num row col =
    isRowValid board num row &&
    isColValid board num col &&
    isBoxValid board num row col &&
    isComparativeValid board num row col

-- Substitui um elemento em uma lista 2D
replace2D :: [[a]] -> (Int, Int) -> a -> [[a]]
replace2D matrix (i, j) x = 
    take i matrix ++ [replace (matrix !! i) j x] ++ drop (i+1) matrix
  where
    replace row j x = take j row ++ [x] ++ drop (j+1) row

-- Encontra uma célula vazia no tabuleiro
findEmpty :: Board -> Int -> Int -> Maybe (Int, Int)
findEmpty board row col
    | row == length board = Nothing
    | fst (board !! row !! col) == 0 = Just (row, col)
    | col == (length (board !! 0) - 1) = findEmpty board (row+1) 0
    | otherwise = findEmpty board row (col+1)

-- Tenta preencher uma célula vazia com um número válido
tryNumber :: Board -> Int -> Int -> Int -> Maybe Board
tryNumber board num row col
    | num > length board = Nothing
    | isValid board num row col =
        let newCell = (num, getSecond (board !! row !! col), getThird (board !! row !! col), getFourth (board !! row !! col), getFifth (board !! row !! col))
            newBoard = replace2D board (row, col) newCell
        in solveComparative newBoard
    | otherwise = tryNumber board (num+1) row col

-- Funções auxiliares para obter partes de uma célula
getSecond (_, x, _, _, _) = x
getThird (_, _, x, _, _) = x
getFourth (_, _, _, x, _) = x
getFifth (_, _, _, _, x) = x

-- Resolve o tabuleiro
solveComparative :: Board -> Maybe Board
solveComparative board
    | findEmpty board 0 0 == Nothing = Just board
    | otherwise = let (row, col) = fromJust (findEmpty board 0 0) in tryNumber board 1 row col

-- Exibe uma célula
printCell :: Cell -> IO ()
printCell (value, _, _, _, _) = putStr (show value ++ " ")

-- Exibe o tabuleiro
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
    content <- readFile "tabuleiro.txt"
    let board = readBoard content
    let solution = solveComparative board
    case solution of
         Just solved -> printBoard solved
         Nothing -> putStrLn "No solution found"
