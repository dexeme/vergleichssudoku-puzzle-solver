;; Import libraries
(require 'cl-lib)

;; Definição dos tipos de dados
(defstruct cell
  (first 0)
  (second #\space)
  (third #\space)
  (fourth #\space)
  (fifth #\space))

(defstruct board
  (cells '()))

;; Funções para obter elementos de uma célula
(defun first-elem (cell)
  (cell-first cell))

(defun get-second (cell)
  (cell-second cell))

(defun get-third (cell)
  (cell-third cell))

(defun get-fourth (cell)
  (cell-fourth cell))

(defun get-fifth (cell)
  (cell-fifth cell))

;; para parametrizar e limpar o código
(defun box-size (n)
  (cond ((= n 4) '(2 2))
        ((= n 6) '(3 2))
        ((= n 9) '(3 3))
        (t (error "Invalid board size"))))

;; Verifica se um número não está repetido na linha atual
(defun is-row-valid (board num row)
  (not (cl-find num (mapcar #'first-elem (nth row (board-cells board))))))

;; Verifica se um número não está repetido na coluna atual
(defun is-col-valid (board num col)
  (not (cl-find num (mapcar (lambda (cell) (first-elem (nth col cell))) (board-cells board)))))

;; Verifica se um número não está repetido na subcaixa atual
(defun is-box-valid (board num row col)
  (let* ((box-size (box-size (length (board-cells board))))
         (box-rows (first box-size))
         (box-cols (second box-size))
         (start-row (* (/ row box-rows) box-rows))
         (start-col (* (/ col box-cols) box-cols))
         (box (loop for r from start-row to (+ start-row box-rows -1)
                    append (loop for c from start-col to (+ start-col box-cols -1)
                                 collect (nth c (nth r (board-cells board)))))))
    (not (cl-find num (mapcar #'first-elem box)))))

;; Verifica se um número é válido considerando as comparações
(defun isComparativeValid (board num row col)
  (let* ((cell (nth row (nth col board)))
         (left (first (rest cell)))
         (up (second (rest cell)))
         (right (third (rest cell)))
         (down (fourth (rest cell)))
         (leftVal (if (= col 0) 0 (first (rest (nth (nth row (1- col))))))
         (upVal (if (= row 0) 0 (first (rest (nth (1- row) (nth col board))))))
         (rightVal (if (= col (- (length (nth 0 board)) 1)) 0 (first (rest (nth (nth row (1+ col))))))
         (downVal (if (= row (- (length board) 1)) 0 (first (rest (nth (1+ row) (nth col board))))))
         (checkLeft (if (or (= col 0) (= leftVal 0)) t (cond ((char= left "<") (< num leftVal))
                                                             ((char= left ">") (> num leftVal))
                                                             (t t))))
         (checkUp (if (or (= row 0) (= upVal 0)) t (cond ((char= up "<") (< num upVal))
                                                         ((char= up ">") (> num upVal))
                                                         (t t))))
         (checkRight (if (or (= col (- (length (nth 0 board)) 1)) (= rightVal 0)) t (cond ((char= right "<") (< num rightVal))
                                                                                           ((char= right ">") (> num rightVal))
                                                                                           (t t))))
         (checkDown (if (or (= row (- (length board) 1)) (= downVal 0)) t (cond ((char= down "<") (< num downVal))
                                                                               ((char= down ">") (> num downVal))
                                                                               (t t)))))
    (and checkLeft checkUp checkRight checkDown)))

;; Verifica se um número é válido considerando todas as regras
(defun isValid (board num row col)
  (and (isRowValid board num row)
       (isColValid board num col)
       (isBoxValid board num row col)
       (isComparativeValid board num row col)))

;; Substitui um elemento em uma lista 2D
(defun replace2D (matrix i j x)
  (let ((row (nth i matrix)))
    (setf (nth i matrix) (replace row j x))
    matrix))

;; Encontra uma célula vazia no tabuleiro
(defun findEmpty (board row col)
  (cond ((= row (length board)) nil)
        ((= (firstElem (nth row (nth col board))) 0) (cons row col))
        ((= col (- (length (nth 0 board)) 1)) (findEmpty board (+ row 1) 0))
        (t (findEmpty board row (+ col 1)))))

;; Tenta preencher uma célula vazia com um número válido
(defun tryNumber (board num row col)
  (cond ((> num (length board)) nil)
        ((isValid board num row col)
         (let ((newCell (list num (second (nth (nth board row) col))
                                (third (nth (nth board row) col))
                                (fourth (nth (nth board row) col))
                                (fifth (nth (nth board row) col)))))
           (let ((newBoard (replace2D board (list row col) newCell))
                 (nextAttempt (solveComparative newBoard)))
             (if (null nextAttempt)
                 (tryNumber board (+ num 1) row col)
                 nextAttempt))))
        (t (tryNumber board (+ num 1) row col))))

;; Resolve o tabuleiro
(defun solveComparative (board)
  (if (null (findEmpty board 0 0))
      (list board)
      (let* ((emptyCell (findEmpty board 0 0))
             (row (first emptyCell))
             (col (second emptyCell)))
        (tryNumber board 1 row col))))

;; Funções de exibição
(defun print-cell (cell)
  (let ((value (first cell)))
    (princ (format nil "~a " value))))
(defun print-board (board)
  (dolist (row board)
    (mapc #'print-cell row)
    (terpri)))
;; Lê o tabuleiro de uma string
(defun read-board (input)
  (read-from-string input))
;; Função principal
(defun main ()
  (let ((content (with-open-file (stream "tabuleiro.txt" :direction :input)
                   (read-line stream))))
    (let ((board (read-board content)))
      (let ((solution (solve-comparative board)))
        (if solution
            (print-board solution)
            (princ "No solution found"))))))
