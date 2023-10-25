(require 'cl-lib)

;; Definição dos tipos de dados
(defstruct cell
  (first-elem nil :type integer)
  (second-elem nil :type character)
  (third-elem nil :type character)
  (fourth-elem nil :type character)
  (fifth-elem nil :type character))

(defstruct board
  (cells nil :type list))

;; Funções para obter elementos de uma célula
(defun first-elem (cell)
  (cell-first-elem cell))

(defun second-elem (cell)
  (cell-second-elem cell))

(defun third-elem (cell)
  (cell-third-elem cell))

(defun fourth-elem (cell)
  (cell-fourth-elem cell))

(defun fifth-elem (cell)
  (cell-fifth-elem cell))

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
  (not (cl-find num (mapcar (lambda (cell) (first-elem cell)) (mapcar (lambda (row) (nth col row)) (board-cells board))))))

;; Verifica se um número não está repetido na subcaixa atual
(defun is-box-valid (board num row col)
  (let* ((box-size (box-size (length (board-cells board))))
         (box-rows (first box-size))
         (box-cols (second box-size))
         (start-row (* (floor (/ row box-rows)) box-rows))
         (start-col (* (floor (/ col box-cols)) box-cols))
         (box (loop for r from start-row to (+ start-row box-rows -1)
                    append (loop for c from start-col to (+ start-col box-cols -1)
                                 collect (nth c (nth r (board-cells board)))))))
    (not (cl-find num (mapcar #'first-elem box)))))

;; Verifica se um número é válido considerando as comparações
(defun is-comparative-valid (board num row col)
  (and (is-row-valid board num row)
       (is-col-valid board num col)
       (is-box-valid board num row col)))
