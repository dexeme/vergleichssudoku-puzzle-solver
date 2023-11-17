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

;; Determina o tamanho das subcaixas 
;; com base no tamanho do tabuleiro
(defun box-size (n)
  (cond ((= n 4) '(2 2))
        ((= n 6) '(3 2))
        ((= n 9) '(3 3))
        (t (error "Invalid board size~%"))
  )
)

(defun deep-copy-cell (cell)
  ; Cria uma cópia profunda de uma célula.
  (make-cell 
    :first (cell-first cell)
    :second (cell-second cell)
    :third (cell-third cell)
    :fourth (cell-fourth cell)
    :fifth (cell-fifth cell)
  )
)

(defun deep-copy-board (board)
  ; Cria uma cópia profunda do tabuleiro.
  (make-board :cells
    (mapcar
      (lambda
        (row)
        (mapcar #'deep-copy-cell row)
      )
      (board-cells board)
    )
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
  (not (member num (mapcar 
    #'cell-first (nth row (board-cells board))
  )))
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
    (not (member num (mapcar #'cell-first box)))
  )
)

;; Verifica se o número é válido com base nas comparações
(defun isComparativeValid (board num row col)
  (let* 
    (
      (cell (nth col (nth row (board-cells board))))
      (left (cell-second cell))
      (up (cell-third cell))
      (right (cell-fourth cell))
      (down (cell-fifth cell))
      (leftVal 
        (if (= col 0)
        ; then
          0
        ; else
          (cell-first (nth (1- col) (nth row (board-cells board))))
        )
      )
      (upVal
        (if (= row 0)
        ; then
          0
        ; else
          (cell-first (nth col (nth (1- row) (board-cells board))))
        )
      )
      (rightVal 
        (if (= col (- (length (board-cells board)) 1))
        ; then
          0
        ; else
          (cell-first (nth (1+ col) (nth row (board-cells board))))
        )
      )
      (downVal
        (if (= row (- (length (board-cells board)) 1))
        ; then
          0
        ; else
          (cell-first (nth col (nth (1+ row) (board-cells board))))
        )
      )
    )
    (and
      ;; Verificação para cada direção
      (is-valid-comparison left num leftVal)
      (is-valid-comparison up num upVal)
      (is-valid-comparison right num rightVal)
      (is-valid-comparison down num downVal)
    )
  )
)


(defun is-valid-comparison (comparison num adjacent-val)
  ; Verifica se a comparação é válida.
  (cond 
    ;; Retorna verdadeiro se a célula estiver vazia
    ((= adjacent-val 0) t)
    ((and (eq comparison (intern "<")) (< num adjacent-val)) t)
    ((and (eq comparison (intern ">")) (> num adjacent-val)) t)
    ;; Retorna falso se a comparação não for satisfeita
    (t nil)
  )
)


;; Tenta colocar um número em uma célula vazia
(defun tryNumber (board num row col)
    ;; Verifica se o número é válido para a célula atual
   (let ((original-board (deep-copy-board board)))
    (cond ((> num (length (board-cells board))) nil)
          ((isValid board num row col)
            (let 
              ((newCell 
                (make-cell
                  :first num 
                  :second (cell-second (nth col (nth row (board-cells board))))
                  :third (cell-third (nth col (nth row (board-cells board))))
                  :fourth (cell-fourth (nth col (nth row (board-cells board))))
                  :fifth (cell-fifth (nth col (nth row (board-cells board))))
                )
               )
              )
              (let 
                ((newBoard 
                  (make-board :cells
                    (replace2D (board-cells board) row col newCell)
                  )
                 )
                )
                (let ((nextAttempt (solveComparative newBoard)))
                  (if (null nextAttempt)
                      ; Volta ao estado original antes de tentar o próximo número
                      (tryNumber original-board (+ num 1) row col) 
                      nextAttempt
                  )
                )
              )
            )
          )
          (t (tryNumber original-board (1+ num) row col))))
)

;; Encontra a próxima célula vazia no tabuleiro
(defun findEmpty (board row col)
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
  (write-line "")
  (dolist (row (board-cells board))
    (mapc #'printCell row)
    (terpri)
  )
)

;; Lê o tabuleiro de uma string
(defun readBoard (input)
  (let 
    ((cells (mapcar 
              (lambda (row) 
                (mapcar (lambda (cell)
                  (make-cell
                    :first (first cell)
                    :second (second cell)
                    :third (third cell)
                    :fourth (fourth cell)
                    :fifth (fifth cell)
                  ))
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
