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
  (elt cell 1))

(defun get-third (cell)
  (elt cell 2))

(defun get-fourth (cell)
  (elt cell 3))

(defun get-fifth (cell)
  (elt cell 4))

;; para parametrizar e limpar o código
(defun box-size (n)
  (cond ((= n 4) '(2 2))
        ((= n 6) '(3 2))
        ((= n 9) '(3 3))
        (t (error "Invalid board size"))
  )
)

;; Verifica se um número não está repetido na linha atual
(defun isRowValid (board num row)
  (not (member num (mapcar #'first-elem (nth row (board-cells board)))))
)

;; Verifica se um número não está repetido na coluna atual
(defun isColValid (board num col)
  (not (member num (mapcar (lambda (cell) (first-elem (nth col cell))) (board-cells board))))
)

;; Verifica se um número não está repetido na subcaixa atual
(defun isBoxValid (board num row col)
  (let* ((box-size (box-size (length (board-cells board))))
         (box-rows (first box-size))
         (box-cols (second box-size))
         (start-row (* (/ row box-rows) box-rows))
         (start-col (* (/ col box-cols) box-cols))
         (box (loop for r from start-row to (+ start-row box-rows -1)
                    append (loop for c from start-col to (+ start-col box-cols -1)
                                 collect (nth c (nth r (board-cells board)))))))
    (not (member num (mapcar #'first-elem box)))
  )
)

;; Verifica se um número é válido considerando as comparações
(defun isComparativeValid (board num row col)
  (let* ((cell (nth col (nth row board)))
         (left (second cell))
         (up (third cell))
         (right (fourth cell))
         (down (fifth cell))
         (leftVal (if (= col 0) 0 (first-elem (nth (1- col) (nth row board)))))
         (upVal (if (= row 0) 0 (first-elem (nth col (nth (1- row) board)))))
         (rightVal (if (= col (- (length (nth 0 board)) 1)) 0 (first-elem (nth (1+ col) (nth row board)))))
         (downVal (if (= row (- (length board) 1)) 0 (first-elem (nth col (nth (1+ row) board))))))
    (and
     (or (= col 0) (= leftVal 0) (and (char= left #\<) (< num leftVal)) (and (char= left #\>) (> num leftVal)))
     (or (= row 0) (= upVal 0) (and (char= up #\<) (< num upVal)) (and (char= up #\>) (> num upVal)))
     (or (= col (- (length (nth 0 board)) 1)) (= rightVal 0) (and (char= right #\<) (< num rightVal)) (and (char= right #\>) (> num rightVal)))
     (or (= row (- (length board) 1)) (= downVal 0) (and (char= down #\<) (< num downVal)) (and (char= down #\>) (> num downVal)))
    )
  )
)

;; Verifica se um número é válido considerando todas as regras
(defun isValid (board num row col)
  (and (isRowValid board num row)
       (isColValid board num col)
       (isBoxValid board num row col)
       (isComparativeValid board num row col)
  )
)

;; Substitui um elemento em uma lista 2D
(defun replace2D (matrix i j x)
  (let ((row (nth i matrix)))
    (setf (nth j row) x)
    matrix)
)

;; Encontra uma célula vazia no tabuleiro
(defun findEmpty (board row col)
  (cond ((= row (length board)) nil)
        ((= (first-elem (nth col (nth row board))) 0) (cons row col))
        ((= col (- (length (nth 0 board)) 1)) (findEmpty board (+ row 1) 0))
        (t (findEmpty board row (+ col 1)))
  )
)

;; Tenta preencher uma célula vazia com um número válido
(defun tryNumber (board num row col)
  (cond 
    ((> num (length board)) nil)
    ((isValid board num row col)
     (let* ((newCell (list num (second (nth col (nth row board)))
                          (third (nth col (nth row board)))
                          (fourth (nth col (nth row board)))
                          (fifth (nth col (nth row board)))))
           (newBoard (replace2D board row col newCell))
           (nextAttempt (solveComparative newBoard)))
       (if (null nextAttempt)
           (tryNumber newBoard (+ num 1) row col)
           nextAttempt)))
    (t (tryNumber board (+ num 1) row col))
  )
)



;; Resolve o tabuleiro
(defun solveComparative (board)
  (if (null (findEmpty board 0 0))
      (list board)
      (let* ((emptyCell (findEmpty board 0 0))
             (row (car emptyCell))
             (col (cdr emptyCell)))
        (tryNumber board 1 row col)
      )
  )
)

;; Funções de exibição
(defun printCell (cell)
  (let ((value (first cell)))
    (princ (format nil "~a " value))
  )
)

(defun printBoard (board)
  (dolist (row board)
    (mapc #'printCell row)
    (terpri)
  )
)

;; Lê o tabuleiro de uma string
(defun readBoard (input)
  (read-from-string input)
)

;; Função principal
(defun main ()
  (let ((content (with-open-file (stream "../tabuleiro.txt" :direction :input)
                   (read-line stream)))
        )
    (let ((board (readBoard content)))
      (let ((solution (solveComparative board)))
        (if solution
            (printBoard solution)
            (princ "No solution found")
        )
      )
    )
  )
)
