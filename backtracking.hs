import Data.List (notElem, elem)
import Data.Maybe (fromJust)

type Cell = (Int, Char, Char, Char, Char)
type Board = [[Cell]]

-- Verifica se um número é válido na linha atual
isRowValid :: Board -> Int -> Int -> Bool
isRowValid board num row = notElem num (map (\(x, _, _, _, _) -> x) (board !! row))

-- Verifica se um número é válido na coluna atual
isColValid :: Board -> Int -> Int -> Bool
isColValid board num col = notElem num [(\(x, _, _, _, _) -> x) (board !! r !! col) | r <- [0..length board - 1]]

-- Verifica se um número é válido na subcaixa atual
isBoxValid :: Board -> Int -> Int -> Int -> Bool
isBoxValid board num row col =
    let boxSize = if (length board == 4) then 2 else 3
        startRow = (row `div` boxSize) * boxSize
        startCol = (col `div` boxSize) * boxSize
        box = [board !! r !! c | r <- [startRow..(startRow + boxSize - 1)], c <- [startCol..(startCol + boxSize - 1)]]
    in notElem num (map (\(x, _, _, _, _) -> x) box)

-- Verifica se um número é válido, considerando as regras do Sudoku e as comparações
isValid :: Board -> Int -> Int -> Int -> Bool
isValid board num row col =
    let (_, left, up, right, down) = board !! row !! col
    in isRowValid board num row &&
       isColValid board num col &&
       isBoxValid board num row col &&
       (left == '/' || (left == '<' && (col == 0 || num < (\(x, _, _, _, _) -> x) (board !! row !! (col-1)))) || (left == '>' && (col == 0 || num > (\(x, _, _, _, _) -> x) (board !! row !! (col-1))))) &&
       (up == '/' || (up == '<' && (row == 0 || num < (\(x, _, _, _, _) -> x) (board !! (row-1) !! col))) || (up == '>' && (row == 0 || num > (\(x, _, _, _, _) -> x) (board !! (row-1) !! col)))) &&
       (right == '/' || (right == '<' && (col == 8 || num < (\(x, _, _, _, _) -> x) (board !! row !! (col+1)))) || (right == '>' && (col == 8 || num > (\(x, _, _, _, _) -> x) (board !! row !! (col+1))))) &&
       (down == '/' || (down == '<' && (row == 8 || num < (\(x, _, _, _, _) -> x) (board !! (row+1) !! col))) || (down == '>' && (row == 8 || num > (\(x, _, _, _, _) -> x) (board !! (row+1) !! col))))

-- Encontra uma célula vazia no tabuleiro
findEmpty :: Board -> Int -> Int -> Maybe (Int, Int)
findEmpty board row col
    | row == length board = Nothing
    | (\(x, _, _, _, _) -> x) (board !! row !! col) == 0 = Just (row, col)
    | col == (length (board !! 0) - 1) = findEmpty board (row+1) 0
    | otherwise = findEmpty board row (col+1)

-- Tenta preencher uma célula vazia com um número válido
tryNumber :: Board -> Int -> Int -> Int -> Maybe Board
tryNumber board num row col
    | num > length board = Nothing
    | isValid board num row col =
        let newCell = (num, (\(_, l, _, _, _) -> l) (board !! row !! col), (\(_, _, u, _, _) -> u) (board !! row !! col), (\(_, _, _, r, _) -> r) (board !! row !! col), (\(_, _, _, _, d) -> d) (board !! row !! col))
            newBoard = replace2D board (row, col) newCell
            updatedBoard = solveComparative newBoard
        in case updatedBoard of
             Just _ -> updatedBoard
             Nothing -> tryNumber board (num+1) row col
    | otherwise = tryNumber board (num+1) row col

solveComparative :: Board -> Maybe Board
solveComparative board
    | findEmpty board 0 0 == Nothing = Just board
    | otherwise = let (row, col) = fromJust (findEmpty board 0 0) in tryNumber board 1 row col


-- Substitui um elemento em uma lista 2D
replace2D :: [[a]] -> (Int, Int) -> a -> [[a]]
replace2D matrix (i, j) x = take i matrix ++ [replace (matrix !! i) j x] ++ drop (i+1) matrix
    where replace row j x = take j row ++ [x] ++ drop (j+1) row

printCell :: Cell -> IO ()
printCell (value, _, _, _, _) = putStr (show value ++ " ")

printBoard :: Board -> IO ()
printBoard [] = return ()
printBoard (row:rows) = do
    mapM_ printCell row
    putStrLn "" -- Nova linha após cada linha do tabuleiro
    printBoard rows

readBoard :: String -> Board
readBoard input = read input :: Board


main :: IO ()
main = do
    content <- readFile "tabuleiro.txt"
    let board = readBoard content
    let solution = solveComparative board
    case solution of
         Just solved -> printBoard solved
         Nothing -> putStrLn "No solution found"
