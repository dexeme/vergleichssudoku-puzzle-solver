;; Define a estrutura de dados para célula
(defstruct cell
  (first 0)
  (second #\space)
  (third #\space)
  (fourth #\space)
  (fifth #\space)
)

;; Define a estrutura de dados para o tabuleiro
(defstruct board
  (cells '())
)

;; Retorna o primeiro elemento da célula
(defun first-elem (cell)
  (cell-first cell)
)

;; Retorna o segundo elemento da célula
(defun get-second (cell)
  (cell-second cell)
)

;; Retorna o terceiro elemento da célula
(defun get-third (cell)
  (cell-third cell)
)

;; Retorna o quarto elemento da célula
(defun get-fourth (cell)
  (cell-fourth cell)
)

;; Retorna o quinto elemento da célula
(defun get-fifth (cell)
  (cell-fifth cell)
)

;; Determina o tamanho das subcaixas com base no tamanho do tabuleiro
(defun box-size (n)
  (cond ((= n 4) '(2 2))
        ((= n 6) '(3 2))
        ((= n 9) '(3 3))
        (t (error "Invalid board size"))
  )
)

;; Verifica se o número não se repete na linha especificada
(defun isRowValid (board num row)
  (not (member num (mapcar #'cell-first (nth row (board-cells board)))))
)

;; Verifica se o número não se repete na coluna especificada
(defun isColValid (board num col)
  (not (member num (mapcar (lambda (cell) (cell-first cell))
                           (mapcar (lambda (row) (nth col row)) 
                                   (board-cells board)
                           )
                   )
        )
  )
)

;; Verifica se o número não se repete na subcaixa especificada
(defun isBoxValid (board num row col)
  (let* ((size (box-size (length (board-cells board))))
         (box-rows (first size))
         (box-cols (second size))
         (start-row (* (/ row box-rows) box-rows))
         (start-col (* (/ col box-cols) box-cols))
         (box (loop for r from start-row below (+ start-row box-rows)
                    append (loop for c from start-col below (+ start-col box-cols)
                                 collect (nth c (nth r (board-cells board)))
                            )
              )
         )
    )
    (not (member num (mapcar #'cell-first box)))
  )
)

;; Verifica se o número é válido com base nas comparações
(defun isComparativeValid (board num row col)
  (let* ((cell (nth col (nth row (board-cells board))))
         (left (cell-second cell))
         (up (cell-third cell))
         (right (cell-fourth cell))
         (down (cell-fifth cell))
         (leftVal (if (= col 0)
                      0
                      (cell-first (nth (1- col) (nth row (board-cells board))))
                  )
         )
         (upVal (if (= row 0)
                    0
                    (cell-first (nth col (nth (1- row) (board-cells board))))
                )
         )
         (rightVal (if (= col (- (length (nth 0 (board-cells board))) 1))
                       0
                       (cell-first (nth (1+ col) (nth row (board-cells board))))
                   )
         )
         (downVal (if (= row (- (length (board-cells board)) 1))
                       0
                       (cell-first (nth col (nth (1+ row) (board-cells board))))
                  )
         )
        );; Adicionando log
    (format t "~%Comparative Validity Check: Cell: ~A, Num: ~A, Row: ~A, Col: ~A" cell num row col)
    (format t "~%Comparisons: Left: ~A, Up: ~A, Right: ~A, Down: ~A" left up right down)
    (and
     (or (not (characterp left))
         (= col 0)
         (= leftVal 0)
         (and (char-equal left #\<) (< num leftVal))
         (and (char-equal left #\>) (> num leftVal)))
     (or (not (characterp up))
         (= row 0)
         (= upVal 0)
         (and (char-equal up #\<) (< num upVal))
         (and (char-equal up #\>) (> num upVal)))
     (or (not (characterp right))
         (= col (- (length (nth 0 (board-cells board))) 1))
         (= rightVal 0)
         (and (char-equal right #\<) (< num rightVal))
         (and (char-equal right #\>) (> num rightVal)))
     (or (not (characterp down))
         (= row (- (length (board-cells board)) 1))
         (= downVal 0)
         (and (char-equal down #\<) (< num downVal))
         (and (char-equal down #\>) (> num downVal)))
    )
  )
)

;; Verifica se o número é válido em todas as regras
(defun isValid (board num row col)
  (and (isRowValid board num row)
       (isColValid board num col)
       (isBoxValid board num row col)
       (isComparativeValid board num row col)
  )
)

;; Substitui um elemento em uma matriz 2D
(defun replace2D (matrix i j x)
  (let ((new-row (copy-list (nth i matrix))))
    (setf (nth j new-row) x)
    (setf (nth i matrix) new-row)
    matrix
  )
)

;; Encontra a próxima célula vazia no tabuleiro
(defun findEmpty (board row col)
  (format t "~%Searching Empty Cell at Row: ~A, Col: ~A" row col)
  (cond ((= row (length (board-cells board))) nil)
        ((let ((cell (nth col (nth row (board-cells board)))))
           (= (cell-first cell) 0))
         (cons row col))
        ((= col (- (length (nth 0 (board-cells board))) 1))
         (findEmpty board (+ row 1) 0))
        (t (findEmpty board row (+ col 1)))
  )
)


;; Tenta colocar um número em uma célula vazia
(defun tryNumber (board num row col)
  ;; Log antes da tentativa de preenchimento
  (format t "~%tryNumber called with num: ~A, row: ~A, col: ~A" num row col)

  (let ((currentCell (nth col (nth row (board-cells board)))))
    (if (/= (cell-first currentCell) 0)
        (progn
          ;; Se a célula já está preenchida, procura a próxima célula vazia
          (let ((nextEmpty (findEmpty board row col)))
            (if nextEmpty
                (progn
                  ;; Log quando uma próxima célula vazia é encontrada
                  (format t "~%Next empty cell found at row: ~A, col: ~A" (car nextEmpty) (cdr nextEmpty))
                  (tryNumber board num (car nextEmpty) (cdr nextEmpty)))
                (progn
                  ;; Log quando não há mais células vazias
                  (format t "~%No more empty cells found")
                  nil))))
        ;; Se a célula está vazia, continua com a tentativa
        (cond ((> num (length (board-cells board))) nil)
              ((isValid board num row col)
               (let* ((newCell (make-cell :first num
                                          :second (cell-second currentCell)
                                          :third (cell-third currentCell)
                                          :fourth (cell-fourth currentCell)
                                          :fifth (cell-fifth currentCell)))
                      (newBoard (make-board :cells 
                                            (replace2D (board-cells board) row col newCell))))
                 (let ((nextAttempt (solveComparative newBoard)))
                   (if (null nextAttempt)
                       (progn
                         ;; Log quando a tentativa falha e tenta o próximo número
                         (format t "~%Attempt with num: ~A at row: ~A, col: ~A failed, trying next number" num row col)
                         (tryNumber board (+ num 1) row col))
                       nextAttempt))))
              (t (progn
                   ;; Log quando o número atual não é válido e tenta o próximo
                   (format t "~%Current num: ~A is not valid at row: ~A, col: ~A, trying next number" num row col)
                   (tryNumber board (+ num 1) row col)))))))


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

;; Imprime uma célula
(defun printCell (cell)
  (princ (format nil "~a " (cell-first cell)))
)

;; Imprime o tabuleiro
(defun printBoard (board)
  (dolist (row (board-cells board))
    (mapc #'printCell row)
    (terpri)
  )
)

;; Lê o tabuleiro de uma string
(defun readBoard (input)
  (let ((cells (mapcar (lambda (row) 
                         (mapcar (lambda (cell) 
                                   (make-cell :first (first cell)
                                              :second (second cell)
                                              :third (third cell)
                                              :fourth (fourth cell)
                                              :fifth (fifth cell)
                                   )
                                 )
                                 row
                         ))
                       (read-from-string input)
         ))
       )
    (make-board :cells cells)
  )
)

(defun main ()
  ;; Função principal
  (let ((content (with-open-file (stream "../tabuleiro.txt" :direction :input)
                   (read-line stream))))
    (let ((board (readBoard content)))
      (let ((solution (solveComparative board)))
        (if solution
            (printBoard (first solution))
            (princ "No solution found")
        )
      )
    )
  )
)

(main)
