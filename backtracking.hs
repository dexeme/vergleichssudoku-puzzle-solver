{--
1. A função solveComparative é chamada para resolver o tabuleiro.
2. Ela verifica se há alguma célula vazia usando a função findEmpty.
3. Se não houver células vazias, o tabuleiro é considerado resolvido e é retornado.
4. Se houver uma célula vazia, ela tentará preencher essa célula
   com um número válido usando a função tryNumber.
5. A função tryNumber tentará preencher a célula com um número de 1 até o tamanho do tabuleiro.
6. Para cada número, ela verifica se o número é válido usando a função isValid.
7. Se o número for válido, ela atualiza o tabuleiro e tenta
   resolver o tabuleiro atualizado chamando solveComparative recursivamente.
8. Se a tentativa de preencher a célula falhar para todos os números,
   ela retorna Nothing, indicando que não foi possível resolver o tabuleiro.
--}

import Data.List (notElem)
import Data.Maybe (fromJust)

-- Funções de Debug
import Debug.Trace-- Funções de debug

debug :: String -> a -> a
debug msg val = trace msg val

debugTryNumber :: Int -> Int -> Int -> Bool
debugTryNumber num row col = debug ("Tentando o número " ++ show num ++ " na posição (" ++ show row ++ "," ++ show col ++ ")") True

debugIsValid :: Int -> Int -> Int -> Bool
debugIsValid num row col = debug ("Número " ++ show num ++ " é válido em (" ++ show row ++ "," ++ show col ++ ")") True

debugNotValid :: Int -> Int -> Int -> Bool
debugNotValid num row col = debug ("Número " ++ show num ++ " não é válido em (" ++ show row ++ "," ++ show col ++ ")") True

debugTryFillCell :: Int -> Int -> Bool
debugTryFillCell row col = debug ("Tentando preencher a célula: (" ++ show row ++ "," ++ show col ++ ")") True

-- Função para debugar comparações
debugComparative :: Char -> Int -> Int -> Bool -> Bool
debugComparative op num comparedVal condition =
    debug (debugComparativeStr op num comparedVal condition) condition

-- Função para formatar a mensagem de debug para comparações
debugComparativeStr :: Char -> Int -> Int -> Bool -> String
debugComparativeStr op num comparedVal condition =
    "Comparing " ++ show num ++ " " ++ [op] ++ " " ++ show comparedVal ++ ": " ++ show condition

-- Fim das Funções de Debug

type Cell = (Int, Char, Char, Char, Char)
type Board = [[Cell]]

-- Função auxiliar para obter o primeiro elemento de uma célula
firstElem :: Cell -> Int
firstElem (x, _, _, _, _) = x
-- Funções auxiliares para obter partes de uma célula
getSecond (_, x, _, _, _) = x
getThird (_, _, x, _, _) = x
getFourth (_, _, _, x, _) = x
getFifth (_, _, _, _, x) = x

-- Verifica se um número é válido na linha atual
isRowValid :: Board -> Int -> Int -> Bool
isRowValid board num row = notElem num (map firstElem (board !! row))

-- Verifica se um número é válido na coluna atual
isColValid :: Board -> Int -> Int -> Bool
isColValid board num col = notElem num [firstElem (board !! r !! col) | r <- [0..length board - 1]]

-- Verifica se um número é válido na subcaixa atual
isBoxValid :: Board -> Int -> Int -> Int -> Bool
isBoxValid board num row col =
    let boxRows = if length board == 4 then 2 else if length board == 6 then 3 else 3
        boxCols = if length board == 4 then 2 else if length board == 6 then 2 else 3
        startRow = (row `div` boxRows) * boxRows
        startCol = (col `div` boxCols) * boxCols
        box = [board !! r !! c | r <- [startRow..(startRow + boxRows - 1)], c <- [startCol..(startCol + boxCols - 1)]]
    in notElem num (map firstElem box)

-- Verifica se um número é válido considerando as comparações
isComparativeValid :: Board -> Int -> Int -> Int -> Bool
isComparativeValid board num row col =
    let (_, left, up, right, down) = board !! row !! col
        leftVal  = if col == 0 then 0 else firstElem (board !! row !! (col-1))
        upVal    = if row == 0 then 0 else firstElem (board !! (row-1) !! col)
        rightVal = if col == (length (board !! 0) - 1) then 0 else firstElem (board !! row !! (col+1))
        downVal  = if row == (length board - 1) then 0 else firstElem (board !! (row+1) !! col)
    
    in debugComparative '<' num leftVal (left == '<' && num < leftVal) &&
       debugComparative '>' num leftVal (left == '>' && num > leftVal) &&
       debugComparative '<' num upVal (up == '<' && num < upVal) &&
       debugComparative '>' num upVal (up == '>' && num > upVal) &&
       debugComparative '<' num rightVal (right == '<' && num < rightVal) &&
       debugComparative '>' num rightVal (right == '>' && num > rightVal) &&
       debugComparative '<' num downVal (down == '<' && num < downVal) &&
       debugComparative '>' num downVal (down == '>' && num > downVal) &&

       (left == '/' || (left == '<' && num < leftVal) || (left == '>' && num > leftVal)) &&
       (up == '/' || (up == '<' && num < upVal) || (up == '>' && num > upVal)) &&
       (right == '/' || (right == '<' && num < rightVal) || (right == '>' && num > rightVal)) &&
       (down == '/' || (down == '<' && num < downVal) || (down == '>' && num > downVal))

-- Verifica se um número é válido considerando todas as regras
isValid :: Board -> Int -> Int -> Int -> Bool
isValid board num row col =
    let rowValid = isRowValid board num row
        colValid = isColValid board num col
        boxValid = isBoxValid board num row col
        compValid = isComparativeValid board num row col
    in if not rowValid then debug "Row invalid" False
       else if not colValid then debug "Col invalid" False
       else if not boxValid then debug "Box invalid" False
       else if not compValid then debug "Comparison invalid" False
       else True

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
    | firstElem (board !! row !! col) == 0 = Just (row, col)
    | col == (length (board !! 0) - 1) = findEmpty board (row+1) 0
    | otherwise = findEmpty board row (col+1)

-- Tenta preencher uma célula vazia com um número válido
tryNumber :: Board -> Int -> Int -> Int -> Maybe Board
tryNumber board num row col
    | num > length board = Nothing
    | otherwise = 
        if debugTryNumber num row col then
            if isValid board num row col then
                let newCell = (num, getSecond (board !! row !! col),
                                    getThird (board !! row !! col),
                                    getFourth (board !! row !! col),
                                    getFifth (board !! row !! col))
                    newBoard = replace2D board (row, col) newCell
                in solveComparative newBoard
            else tryNumber board (num+1) row col
        else tryNumber board (num+1) row col



-- Resolve o tabuleiro
solveComparative :: Board -> Maybe Board
solveComparative board
    | findEmpty board 0 0 == Nothing = Just board
    | otherwise = 
        let (row, col) = fromJust (findEmpty board 0 0)
        in if debugTryFillCell row col then tryNumber board 1 row col
           else Nothing


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
