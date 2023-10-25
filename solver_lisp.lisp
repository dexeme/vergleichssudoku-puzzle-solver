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

