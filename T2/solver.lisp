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

;; Determina o tamanho das subcaixas com base no tamanho do tabuleiro
(defun box-size (n)
  (cond ((= n 4) '(2 2))
        ((= n 6) '(3 2))
        ((= n 9) '(3 3))
        (t (error "Invalid board size~%"))
  )
)

;; Substitui um elemento em uma matriz 2D
(defun replace2D (matrix i j x)
  (format t "~%Replace2D called with row: ~A, col: ~A, num: ~A~%" i j (cell-first x))
  (finish-output)
  (let ((new-row (copy-list (nth i matrix))))
    (setf (nth j new-row) x)
    (setf (nth i matrix) new-row)
    matrix
  )
)

;; Verifica se o número é válido em todas as regras
(defun isValid (board num row col)
  (and
    (isRowValid board num row)
    (isColValid board num col)
    (isBoxValid board num row col)
    (isComparativeValid board num row col)
  )
)

;; Verifica se o número não se repete na linha especificada
(defun isRowValid (board num row)
  (not (member num (mapcar #'cell-first (nth row (board-cells board)))))
)

;; Verifica se o número não se repete na coluna especificada
(defun isColValid (board num col)
  (not (member num (mapcar 
    (lambda (cell) (cell-first cell))
    (mapcar
      (lambda (row) (nth col row))
      (board-cells board)
    )
  )))
)

;; Verifica se o número não se repete na subcaixa especificada
(defun isBoxValid (board num row col)
  (format t "~%isBoxValid with num: ~A, row: ~A, col: ~A~%" num row col)
  (finish-output)
  (let*
    ( 
      (box_size (box-size (length (board-cells board))))
      (box-rows (first box_size))
      (box-cols (second box_size))
      ; (start-row (* (/ row box-rows) box-rows))
      ; (start-col (* (/ col box-cols) box-cols))
      (start-row (* (floor row box-rows) box-rows))
      (start-col (* (floor col box-cols) box-cols))
      ;; Problema Aqui, gera células uma célula válida e uma NIL *2
      (box  (loop for r from start-row below (+ start-row box-rows)
              append  (loop for c from start-col below (+ start-col box-cols)
                        collect (nth c (nth r (board-cells board)))
                      )
            )
      )

    )
    (format t "~%Box: ~A~%----~%" box)
    (format t "start-row: ~A, start-col: ~A~%" start-row start-col)
    (not (member num (mapcar #'cell-first box)))
  )
)

;; Verifica se o número é válido com base nas comparações
(defun isComparativeValid (board num row col)
  (format t "~%--------~%isComparativeValid with Num: ~A, Row: ~A, Col: ~A" num row col)
  (finish-output)
  (let* ((cell (nth col (nth row (board-cells board))))
         (left (cell-second cell))
         (up (cell-third cell))
         (right (cell-fourth cell))
         (down (cell-fifth cell))
         (leftVal (if (= col 0)
                    ; then
                    0
                    ; else
                    (cell-first (nth (1- col) (nth row (board-cells board))))
                  )
         )
         (upVal (if (= row 0)
                    ; then
                    0
                    ; else
                  (cell-first (nth col (nth (1- row) (board-cells board))))
                )
         )
         (rightVal (if (= col (- (length (nth 0 (board-cells board))) 1))
                    ; then
                    0
                    ; else
                      (cell-first (nth (1+ col) (nth row (board-cells board))))
                   )
         )
         (downVal (if (= row (- (length (board-cells board)) 1))
                    ; then
                    0
                    ; else
                      (cell-first (nth col (nth (1+ row) (board-cells board))))
                  )
         )
        )
        (format t "~%Comparisons: Left: ~A, Up: ~A, Right: ~A, Down: ~A~%--------~%" left up right down)
        (finish-output)
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


;; Tenta colocar um número em uma célula vazia
(defun tryNumber (board num row col)
  ;; Log antes da tentativa de preenchimento
  (format t "~%tryNumber with num: ~A, row: ~A, col: ~A~%" num row col)
  (finish-output)
    ;; Verifica se o número é válido para a célula atual
    (cond ((> num (length (board-cells board))) nil)
          ((isValid board num row col)
            (format t "~%Creating newCell with num: ~A at row: ~A, col: ~A~%" num row col)
            (finish-output)
            (let*
                ((currentCell (nth col (nth row (board-cells board))))
                 (newCell 
                  (make-cell :first num
                             :second (cell-second currentCell)
                             :third (cell-third currentCell)
                             :fourth (cell-fourth currentCell)
                             :fifth (cell-fifth currentCell)
                  )
                 )
                 (newBoard (make-board :cells
                              (replace2D (board-cells board) row col newCell)
                           )
                 )
                )
                (let ((nextAttempt (solveComparative newBoard)))
                  (if (null nextAttempt)
                      (tryNumber board (+ num 1) row col)
                      nextAttempt
                  )
                )
            )
          )
          ;; Log quando o número atual não é válido e tenta o próximo
          (t (progn
               (format t "~%~A is not valid at row: ~A, col: ~A, trying next number~%" num row col)
               (finish-output)
               (tryNumber board (+ num 1) row col)
             )
          )
    )
)


;; Encontra a próxima célula vazia no tabuleiro
(defun findEmpty (board row col)
  (format t "findEmpty at Row: ~A, Col: ~A~%" row col)
  (finish-output)
  (cond ((= row (length (board-cells board))) nil)
        ((let ((cell (nth col (nth row (board-cells board)))))
              (= (cell-first cell) 0)
         )
         (cons row col)
        )
        ((= col (- (length (nth 0 (board-cells board))) 1))
         (findEmpty board (+ row 1) 0)
        )
        (t (findEmpty board row (+ col 1)))
  )
)


;; Resolve o tabuleiro
(defun solveComparative (board)
  (write-line "")
  (write-line "------Solving------")
  ; Imprime o tabuleiro após a atualização
  (printBoard board)       
  (write-line "-------------------")
  (finish-output)
  (let ((emptyCell (findEmpty board 0 0)))
    (if (null emptyCell)
        ;; Se não há células vazias, retorna o tabuleiro como solução
        (list board)
        ;; Caso contrário, tenta preencher a célula vazia encontrada
        (let (
          (row (car emptyCell))
          (col (cdr emptyCell))
          )
          (tryNumber board 1 row col)
        )
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
  (let 
    (
      (cells 
        (mapcar 
          (lambda
            (row) 
            (mapcar
              (lambda 
                (cell)
                (make-cell  :first (first cell)
                            :second (second cell)
                            :third (third cell)
                            :fourth (fourth cell)
                            :fifth (fifth cell)
                 )
              )
              row
            )
          )
          (read-from-string input)
        )
      )
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
