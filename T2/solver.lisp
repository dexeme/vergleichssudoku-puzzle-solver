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

;; Parametrização e limpeza do código
(defun box-size (n)
  (cond ((= n 4) '(2 2))
        ((= n 6) '(3 2))
        ((= n 9) '(3 3))
        (t (error "Invalid board size"))))

(defun isRowValid (board num row)
  ;; Verifica se um número não está repetido na linha atual
  (not (member num (mapcar #'cell-first (nth row (board-cells board))))))

(defun isColValid (board num col)
  ;; Verifica se um número não está repetido na coluna atual
  (not (member num (mapcar (lambda (cell) (cell-first cell)) 
                           (mapcar (lambda (row) (nth col row)) 
                                   (board-cells board))))))

(defun isBoxValid (board num row col)
  ;; Verifica se um número não está repetido na subcaixa atual
  (let* ((size (box-size (length (board-cells board))))
         (box-rows (first size))
         (box-cols (second size))
         (start-row (* (/ row box-rows) box-rows))
         (start-col (* (/ col box-cols) box-cols))
         (box (loop for r from start-row below (+ start-row box-rows)
                    append (loop for c from start-col below (+ start-col box-cols)
                                 collect (nth c (nth r (board-cells board)))))))
    (not (member num (mapcar #'cell-first box)))))

(defun isComparativeValid (board num row col)
  ;; Verifica se um número é válido considerando as comparações
  (format t "num: ~A, row: ~A, col: ~A~%" num row col)
  (let* ((cell (nth col (nth row (board-cells board))))
         (left (cell-second cell))
         (up (cell-third cell))
         (right (cell-fourth cell))
         (down (cell-fifth cell))
         (leftVal (if (= col 0) 0 (cell-first (nth (1- col) (nth row (board-cells board))))))
         (upVal (if (= row 0) 0 (cell-first (nth col (nth (1- row) (board-cells board))))))
         (rightVal (if (= col (- (length (nth 0 (board-cells board))) 1)) 0 (cell-first (nth (1+ col) (nth row (board-cells board))))))
         (downVal (if (= row (- (length (board-cells board)) 1)) 0 (cell-first (nth col (nth (1+ row) (board-cells board)))))))
    (and
     (or (= col 0) (= leftVal 0) (and (char-equal left #\<) (< num leftVal)) (and (char-equal left #\>) (> num leftVal)))
     (or (= row 0) (= upVal 0) (and (char-equal up #\<) (< num upVal)) (and (char-equal up #\>) (> num upVal)))
     (or (= col (- (length (nth 0 (board-cells board))) 1)) (= rightVal 0) (and (char-equal right #\<) (< num rightVal)) (and (char-equal right #\>) (> num rightVal)))
     (or (= row (- (length (board-cells board)) 1)) (= downVal 0) (and (char-equal down #\<) (< num downVal)) (and (char-equal down #\>) (> num downVal)))))
)


(defun isValid (board num row col)
  ;; Verifica se um número é válido considerando todas as regras
  (and (isRowValid board num row)
       (isColValid board num col)
       (isBoxValid board num row col)
       (isComparativeValid board num row col)))

(defun replace2D (matrix i j x)
  ;; Substitui um elemento em uma lista 2D
  (let ((new-row (copy-list (nth i matrix))))
    (setf (nth j new-row) x)
    (setf (nth i matrix) new-row)
    matrix))

(defun findEmpty (board row col)
  ;; Encontra uma célula vazia no tabuleiro
  (cond ((= row (length (board-cells board))) nil)
        ((= (cell-first (nth col (nth row (board-cells board)))) 0) (cons row col))
        ((= col (- (length (nth 0 (board-cells board))) 1)) (findEmpty board (+ row 1) 0))
        (t (findEmpty board row (+ col 1)))))

(defun tryNumber (board num row col)
  ;; Tenta preencher uma célula vazia com um número válido
  (cond ((> num (length (board-cells board))) nil)
        ((isValid board num row col)
         (let* ((currentCell (nth col (nth row (board-cells board))))
                (newCell (make-cell :first num
                                    :second (cell-second currentCell)
                                    :third (cell-third currentCell)
                                    :fourth (cell-fourth currentCell)
                                    :fifth (cell-fifth currentCell)))
                (newBoard (make-board :cells (replace2D (board-cells board) row col newCell)))
                (nextAttempt (solveComparative newBoard)))
           (if (null nextAttempt)
               (tryNumber board (+ num 1) row col)
               nextAttempt)))
        (t (tryNumber board (+ num 1) row col))))

(defun solveComparative (board)
  ;; Resolve o tabuleiro
  (if (null (findEmpty board 0 0))
      (list board)
      (let* ((emptyCell (findEmpty board 0 0))
             (row (car emptyCell))
             (col (cdr emptyCell)))
        (tryNumber board 1 row col))))

(defun printCell (cell)
  ;; Imprime uma célula
  (princ (format nil "~a " (cell-first cell))))

(defun printBoard (board)
  ;; Imprime o tabuleiro
  (dolist (row (board-cells board))
    (mapc #'printCell row)
    (terpri)))

(defun readBoard (input)
  ;; Lê o tabuleiro de uma string
  (let ((cells (mapcar (lambda (row) 
                         (mapcar (lambda (cell) 
                                   (make-cell :first (first cell)
                                              :second (second cell)
                                              :third (third cell)
                                              :fourth (fourth cell)
                                              :fifth (fifth cell)))
                                 row))
                       (read-from-string input))))
    (make-board :cells cells)))

(defun main ()
  ;; Função principal
  (let ((content (with-open-file (stream "../tabuleiro.txt" :direction :input)
                   (read-line stream))))
    (let ((board (readBoard content)))
      (let ((solution (solveComparative board)))
        (if solution
            (printBoard (first solution))
            (princ "No solution found"))))))

(main)
