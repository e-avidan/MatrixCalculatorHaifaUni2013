(ns MatrixCalculator.demonstration
  (:use MatrixCalculator.Functions))

;;;; REPL mode
;;   This file contains instructions on how to implement the functions
;;   we created. Each function saves it's output in a variable, for
;;   your comfort in reusing output. Each function is accompanied
;;   by minimal instructions - for more please look at the implementation in
;;   MatrixCalculator.Functions, the tests in MatrixCalculator.core_test,
;;   or the Project Report, in the .rar file.


;;; This file includes many matrices for you to use to test our project
;;  at it's upper part, and function templates in the lower one.
;;  to print a calculation, change the matPrint function (last line)
;;  to the name of the calculation (or print it from the line if you
;;  don't mind that it isn't a user friendly output

;;  You can also use the defined names to function outputs in other functions.


;;;matrices for input

;4x4 identity
(def eye44 [[1 0 0 0] [0 1 0 0] [0 0 1 0] [0 0 0 1]])

;3x4
(def m34 [
          [3 4 5 2]
          [6 3 7 6]
          [5 2 8 9]
          ])

;4x3
(def m43 
  (matrix [2 7 1] [6 5 4.3] [9 3.7 7] [5 7.8 6]))

;5x5
(def m55 [
          [1 6 3 8 2]
          [5 7.7 3 3.3 6]
          [32 1 2 3 4]
          [65 2 8 2 7]
          [-2 -6 3 -65 3]])

;4x4
(def m44 
  (matrix [-5 3 3 -2] [-2.4 4.3 5 3.5] [2 2.1 1.6 80] [5.5 8 99 -4.2]))

;3x3 symmetric
(def m33
  (matrix [1 2 3] [2 5 6] [3 6 7]))

;4x5 single value e
(def m45 (scalarMat Math/E 5 4))

;5x4 single value PI
(def m45 (scalarMat Math/PI 4 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;function templates

;;simple
;constructor
;enter rows of matrix. Will return nil if input does not
;specify a matrix
(def Matrix (matrix [3 2 2] [2 23 3] [3 2 1]))
;predicates
(def isMat (matrix? m34))
(def isSquare (square? m55))
(def EqSizes (equallySized? (list m55 m33 m34 eye44)))
;get column
(def Column (get-col m44 2))
;get row
(def Row (get-row m34 1))
;# of columns
(def Columns (cols m55))
;# of rows
(def Rows (rows m43))

;;element-by-element, must be same sized matrices!
;Addition
(def Addition (matFunc + m44 m44 m44 m44 m44 m44))
;Substraction
(def Substraction (matFunc - m44 m44 m44 m44 m44 m44))
;Point Multiplication
(def PointMul (matFunc * m43 m43 m43 m43 m43 m43))
;Point Division (avoid zeros in any matrix but the first
(def PointDiv (matFunc / m55 m55 m55 m55 m55 m55))
;Point Power (be aware that if you cross E300 you get infinity),
;and NaN can also occur
(def PointPow (matFunc expt m34 m34 m34))

;;Matrix Math
;Matrix Multipication - on each multiplication, cols of first = rows of second
(def MatMul (matMul m43 m34 m44 m43))
;Matrix Power - only square matrices, integer powers > 0
(def MatPow (matPow m44 5))
;ScalarMatrix - parameters are value, #cols, #rows
(def ScalarMatrix (scalarMat -2.718, 5, 3))

;;Single Matrix Operations
;Euclidean Norm
(def Norm (norm2 ScalarMatrix))
;Normalize
(def Normalized (normalize ScalarMatrix))
;Determinant - only square matrices!
(def Determinant (det m55))
;Transpose
(def Transpose (transpose m33))
;SetIndex - parameters are matrix, index, value
(def NewMat (setInd m44 7 3.1415))
(matFunc + m33 m33)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Print!
(matPrint Addition)